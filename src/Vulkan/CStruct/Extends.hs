{-# language CPP #-}
-- No documentation found for Chapter "Extends"
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
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (join)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AabbPositionsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildGeometryInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildRangeInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildSizesInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (AccelerationStructureCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureDeviceAddressInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureGeometryAabbsDataKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureGeometryInstancesDataKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureGeometryKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (AccelerationStructureGeometryMotionTrianglesDataNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureGeometryTrianglesDataKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureInstanceKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (AccelerationStructureMatrixMotionInstanceNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureMemoryRequirementsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (AccelerationStructureMotionInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (AccelerationStructureMotionInstanceNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (AccelerationStructureSRTMotionInstanceNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_displacement_micromap (AccelerationStructureTrianglesDisplacementMicromapNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (AccelerationStructureTrianglesOpacityMicromapEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureVersionInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (AcquireNextImageInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (AcquireProfilingLockInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import {-# SOURCE #-} Vulkan.Extensions.VK_SEC_amigo_profiling (AmigoProfilingSubmitInfoSEC)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AndroidHardwareBufferFormatProperties2ANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AndroidHardwareBufferFormatPropertiesANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_format_resolve (AndroidHardwareBufferFormatResolvePropertiesANDROID)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (AttachmentSampleCountInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (AttachmentSampleLocationsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (BindAccelerationStructureMemoryInfoNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindBufferMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindBufferMemoryInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindImageMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindImageMemoryInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (BindImageMemorySwapchainInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (BindImagePlaneMemoryInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (BindIndexBufferIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands_compute (BindPipelineIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (BindShaderGroupIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (BindSparseInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (BindVertexBufferIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_filter_cubic_weights (BlitImageCubicWeightsInfoQCOM)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (BlitImageInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (BufferCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionBufferCreateInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionConstraintsInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionCreateInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionImageCreateInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionPropertiesFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferConstraintsInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (BufferCopy)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (BufferCopy2)
import {-# SOURCE #-} Vulkan.Core10.Buffer (BufferCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (BufferDeviceAddressCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (BufferImageCopy)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (BufferImageCopy2)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (BufferMemoryBarrier)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (BufferMemoryBarrier2)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (BufferMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferOpaqueCaptureAddressCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (BufferUsageFlags2CreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.BufferView (BufferViewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_calibrated_timestamps (CalibratedTimestampInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_synchronization2 (CheckpointData2NV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints (CheckpointDataNV)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearAttachment)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearDepthStencilValue)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearRect)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (CoarseSampleLocationNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (CoarseSampleOrderCustomNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (ColorBlendAdvancedEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (ColorBlendEquationEXT)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferAllocateInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferBeginInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (CommandBufferInheritanceConditionalRenderingInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferInheritanceInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_render_pass_transform (CommandBufferInheritanceRenderPassTransformInfoQCOM)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (CommandBufferInheritanceRenderingInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_inherited_viewport_scissor (CommandBufferInheritanceViewportScissorInfoNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (CommandBufferSubmitInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandPool (CommandPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ComponentMapping)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (ComputePipelineCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands_compute (ComputePipelineIndirectBufferInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (ConditionalRenderingBeginInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (ConformanceVersion)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_cooperative_matrix (CooperativeMatrixPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (CooperativeMatrixPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureToMemoryInfoKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyBufferInfo2)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyBufferToImageInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_rotated_copy_commands (CopyCommandTransformInfoQCOM)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (CopyDescriptorSet)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyImageInfo2)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyImageToBufferInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (CopyImageToImageInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (CopyImageToMemoryInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_copy_memory_indirect (CopyMemoryIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (CopyMemoryToAccelerationStructureInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_copy_memory_indirect (CopyMemoryToImageIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (CopyMemoryToImageInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (CopyMemoryToMicromapInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (CopyMicromapInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (CopyMicromapToMemoryInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_binary_import (CuFunctionCreateInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_binary_import (CuLaunchInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_binary_import (CuModuleCreateInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cuda_kernel_launch (CudaFunctionCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cuda_kernel_launch (CudaLaunchInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cuda_kernel_launch (CudaModuleCreateInfoNV)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_memory_decompression (DecompressMemoryRegionNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationBufferCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationImageCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationMemoryAllocateInfoNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (DependencyInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_bias_control (DepthBiasInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_bias_control (DepthBiasRepresentationInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (DescriptorAddressInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (DescriptorBufferBindingInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (DescriptorBufferBindingPushDescriptorBufferHandleEXT)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorBufferInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (DescriptorGetInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorImageInfo)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (DescriptorPoolInlineUniformBlockCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorPoolSize)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping (DescriptorSetBindingReferenceVALVE)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetLayoutBinding)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetLayoutBindingFlagsCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetLayoutCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping (DescriptorSetLayoutHostMappingInfoVALVE)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (DescriptorSetLayoutSupport)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountAllocateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountLayoutSupport)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateEntry)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_address_binding_report (DeviceAddressBindingCallbackDataEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (DeviceBufferMemoryRequirements)
import {-# SOURCE #-} Vulkan.Core10.Device (DeviceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (DeviceDeviceMemoryReportCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (DeviceDiagnosticsConfigCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (DeviceEventInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (DeviceFaultAddressInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (DeviceFaultCountsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (DeviceFaultInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (DeviceFaultVendorBinaryHeaderVersionOneEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (DeviceFaultVendorInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupBindSparseInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupCommandBufferBeginInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (DeviceGroupDeviceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupSubmitInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupSwapchainCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (DeviceImageMemoryRequirements)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (DeviceImageSubresourceInfoKHR)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (DeviceMemoryOpaqueCaptureAddressInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_memory_overallocation_behavior (DeviceMemoryOverallocationCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (DeviceMemoryReportCallbackDataEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_private_data (DevicePrivateDataCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Device (DeviceQueueCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_global_priority (DeviceQueueGlobalPriorityCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (DeviceQueueInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_scheduling_controls (DeviceQueueShaderCoreControlCreateInfoARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_LUNARG_direct_driver_loading (DirectDriverLoadingInfoLUNARG)
import {-# SOURCE #-} Vulkan.Extensions.VK_LUNARG_direct_driver_loading (DirectDriverLoadingListLUNARG)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_directfb_surface (DirectFBSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (DispatchGraphCountInfoAMDX)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (DispatchGraphInfoAMDX)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mesh_shader (DrawMeshTasksIndirectCommandEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (DrawMeshTasksIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierProperties2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierPropertiesList2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierPropertiesListEXT)
import {-# SOURCE #-} Vulkan.Core10.Event (EventCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (ExecutionGraphPipelineCreateInfoAMDX)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (ExecutionGraphPipelineScratchSizeAMDX)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence (ExportFenceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (ExportFenceWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExportMemoryAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory (ExportMemoryAllocateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (ExportMemoryWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_win32 (ExportMemoryWin32HandleInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalBufferInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalCommandQueueInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalDeviceInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalIOSurfaceInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalObjectCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalObjectsInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalSharedEventInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalTextureInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore (ExportSemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ExportSemaphoreWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.ExtensionDiscovery (ExtensionProperties)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Extent2D)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Extent3D)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalBufferProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (ExternalFenceProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_external_memory_screen_buffer (ExternalFormatQNX)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalImageFormatProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalImageFormatPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_acquire_unmodified (ExternalMemoryAcquireUnmodifiedEXT)
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
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_format_feature_flags2 (FormatProperties3)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateAttachmentInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_frame_boundary (FrameBoundaryEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (FramebufferAttachmentImageInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (FramebufferAttachmentsCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (FramebufferCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (FramebufferMixedSamplesCombinationNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GeneratedCommandsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GeneratedCommandsMemoryRequirementsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryAABBNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryDataNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryTrianglesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (GetLatencyMarkerInfoNV)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (GraphicsPipelineCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_graphics_pipeline_library (GraphicsPipelineLibraryCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GraphicsPipelineShaderGroupsCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GraphicsShaderGroupCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_hdr_metadata (HdrMetadataEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_headless_surface (HeadlessSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (HostImageCopyDevicePerformanceQueryEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (HostImageLayoutTransitionInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_MVK_ios_surface (IOSSurfaceCreateInfoMVK)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageBlit)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageBlit2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (ImageCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (ImageCompressionControlEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (ImageCompressionPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (ImageConstraintsInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageCopy)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageCopy2)
import {-# SOURCE #-} Vulkan.Core10.Image (ImageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierExplicitCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierListCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (ImageFormatConstraintsInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (ImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (ImageFormatProperties2)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (ImageMemoryBarrier)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (ImageMemoryBarrier2)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (ImagePipeSurfaceCreateInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (ImagePlaneMemoryRequirementsInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageResolve)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ImageResolve2)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageSparseMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (ImageSubresource2KHR)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ImageSubresourceRange)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (ImageSwapchainCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (ImageToMemoryCopyEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (ImageViewASTCDecodeModeEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_image_view_handle (ImageViewAddressPropertiesNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (ImageViewCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ImageViewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_image_view_handle (ImageViewHandleInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_view_min_lod (ImageViewMinLodCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_image_processing (ImageViewSampleWeightCreateInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_sliced_view_of_3d (ImageViewSlicedCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (ImageViewUsageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ImportAndroidHardwareBufferInfoANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_fd (ImportFenceFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (ImportFenceWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (ImportMemoryBufferCollectionFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (ImportMemoryFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (ImportMemoryHostPointerInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (ImportMemoryWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_win32 (ImportMemoryWin32HandleInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_memory (ImportMemoryZirconHandleInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalBufferInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalIOSurfaceInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalSharedEventInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalTextureInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_external_memory_screen_buffer (ImportScreenBufferInfoQNX)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_fd (ImportSemaphoreFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ImportSemaphoreWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_semaphore (ImportSemaphoreZirconHandleInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsLayoutCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsLayoutTokenNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsStreamNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (InitializePerformanceApiInfoINTEL)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (InputAttachmentAspectReference)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (InstanceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (LatencySleepInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (LatencySleepModeInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (LatencySubmissionPresentIdNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (LatencySurfaceCapabilitiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (LatencyTimingsFrameReportNV)
import {-# SOURCE #-} Vulkan.Core10.LayerDiscovery (LayerProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_layer_settings (LayerSettingEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_layer_settings (LayerSettingsCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_MVK_macos_surface (MacOSSurfaceCreateInfoMVK)
import {-# SOURCE #-} Vulkan.Core10.Memory (MappedMemoryRange)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (MemoryAllocateFlagsInfo)
import {-# SOURCE #-} Vulkan.Core10.Memory (MemoryAllocateInfo)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (MemoryBarrier)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (MemoryBarrier2)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedAllocateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedRequirements)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (MemoryFdPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (MemoryGetAndroidHardwareBufferInfoANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (MemoryGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_rdma (MemoryGetRemoteAddressInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (MemoryGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_memory (MemoryGetZirconHandleInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (MemoryHeap)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (MemoryHostPointerPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_map_memory2 (MemoryMapInfoKHR)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (MemoryOpaqueCaptureAddressAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (MemoryPriorityAllocateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.MemoryManagement (MemoryRequirements)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (MemoryToImageCopyEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (MemoryType)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_map_memory2 (MemoryUnmapInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (MemoryWin32HandlePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_memory (MemoryZirconHandlePropertiesFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_surface (MetalSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapBuildInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapBuildSizesInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapTriangleEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapUsageEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapVersionInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (MultiDrawIndexedInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (MultiDrawInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (MultisamplePropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled (MultisampledRenderToSingleSampledInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (MultiviewPerViewAttributesInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas (MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mutable_descriptor_type (MutableDescriptorTypeCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mutable_descriptor_type (MutableDescriptorTypeListEXT)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Offset2D)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Offset3D)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (OpaqueCaptureDescriptorDataCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowExecuteInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowImageFormatInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowImageFormatPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowSessionCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowSessionCreatePrivateDataInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (OutOfBandQueueTypeInfoNV)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (PhysicalDeviceAccelerationStructureFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (PhysicalDeviceAccelerationStructurePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_address_binding_report (PhysicalDeviceAddressBindingReportFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_SEC_amigo_profiling (PhysicalDeviceAmigoProfilingFeaturesSEC)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state (PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_attachment_feedback_loop_layout (PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_border_color_swizzle (PhysicalDeviceBorderColorSwizzleFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader (PhysicalDeviceClusterCullingShaderFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader (PhysicalDeviceClusterCullingShaderPropertiesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_cluster_culling_shader (PhysicalDeviceClusterCullingShaderVrsFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_device_coherent_memory (PhysicalDeviceCoherentMemoryFeaturesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_color_write_enable (PhysicalDeviceColorWriteEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (PhysicalDeviceConditionalRenderingFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (PhysicalDeviceConservativeRasterizationPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_cooperative_matrix (PhysicalDeviceCooperativeMatrixPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_copy_memory_indirect (PhysicalDeviceCopyMemoryIndirectFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_copy_memory_indirect (PhysicalDeviceCopyMemoryIndirectPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_corner_sampled_image (PhysicalDeviceCornerSampledImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PhysicalDeviceCoverageReductionModeFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_filter_cubic_clamp (PhysicalDeviceCubicClampFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_filter_cubic_weights (PhysicalDeviceCubicWeightsFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cuda_kernel_launch (PhysicalDeviceCudaKernelLaunchFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cuda_kernel_launch (PhysicalDeviceCudaKernelLaunchPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_bias_control (PhysicalDeviceDepthBiasControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clamp_zero_one (PhysicalDeviceDepthClampZeroOneFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_control (PhysicalDeviceDepthClipControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PhysicalDeviceDepthClipEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (PhysicalDeviceDepthStencilResolveProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (PhysicalDeviceDescriptorBufferFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (PhysicalDeviceDescriptorBufferPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_descriptor_pool_overallocation (PhysicalDeviceDescriptorPoolOverallocationFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping (PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands_compute (PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (PhysicalDeviceDeviceMemoryReportFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (PhysicalDeviceDiagnosticsConfigFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_discard_rectangles (PhysicalDeviceDiscardRectanglePropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_displacement_micromap (PhysicalDeviceDisplacementMicromapFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_displacement_micromap (PhysicalDeviceDisplacementMicromapPropertiesNV)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (PhysicalDeviceDriverProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_physical_device_drm (PhysicalDeviceDrmPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_dynamic_rendering_unused_attachments (PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PhysicalDeviceExclusiveScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state2 (PhysicalDeviceExtendedDynamicState2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (PhysicalDeviceExtendedDynamicState3FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (PhysicalDeviceExtendedDynamicState3PropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_extended_sparse_address_space (PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_extended_sparse_address_space (PhysicalDeviceExtendedSparseAddressSpacePropertiesNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalBufferInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (PhysicalDeviceExternalFenceInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_format_resolve (PhysicalDeviceExternalFormatResolveFeaturesANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_format_resolve (PhysicalDeviceExternalFormatResolvePropertiesANDROID)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalImageFormatInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (PhysicalDeviceExternalMemoryHostPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_rdma (PhysicalDeviceExternalMemoryRDMAFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_external_memory_screen_buffer (PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (PhysicalDeviceExternalSemaphoreInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (PhysicalDeviceFaultFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls (PhysicalDeviceFloatControlsProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2PropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_fragment_density_map_offset (PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_fragment_density_map_offset (PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PhysicalDeviceFragmentShadingRateEnumsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PhysicalDeviceFragmentShadingRateEnumsPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRatePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_frame_boundary (PhysicalDeviceFrameBoundaryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_global_priority (PhysicalDeviceGlobalPriorityQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_graphics_pipeline_library (PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_graphics_pipeline_library (PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (PhysicalDeviceGroupProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (PhysicalDeviceHostImageCopyFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (PhysicalDeviceHostImageCopyPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceIDProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_2d_view_of_3d (PhysicalDeviceImage2DViewOf3DFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (PhysicalDeviceImageCompressionControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control_swapchain (PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (PhysicalDeviceImageDrmFormatModifierInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceImageFormatInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_image_processing2 (PhysicalDeviceImageProcessing2FeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_image_processing2 (PhysicalDeviceImageProcessing2PropertiesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_image_processing (PhysicalDeviceImageProcessingFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_image_processing (PhysicalDeviceImageProcessingPropertiesQCOM)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_image_robustness (PhysicalDeviceImageRobustnessFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_sliced_view_of_3d (PhysicalDeviceImageSlicedViewOf3DFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_filter_cubic (PhysicalDeviceImageViewImageFormatInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_view_min_lod (PhysicalDeviceImageViewMinLodFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (PhysicalDeviceImagelessFramebufferFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_inherited_viewport_scissor (PhysicalDeviceInheritedViewportScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockFeatures)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_invocation_mask (PhysicalDeviceInvocationMaskFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_MSFT_layered_driver (PhysicalDeviceLayeredDriverPropertiesMSFT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_legacy_dithering (PhysicalDeviceLegacyDitheringFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceLimits)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_linear_color_attachment (PhysicalDeviceLinearColorAttachmentFeaturesNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (PhysicalDeviceMaintenance3Properties)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (PhysicalDeviceMaintenance4Features)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (PhysicalDeviceMaintenance4Properties)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (PhysicalDeviceMaintenance5FeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (PhysicalDeviceMaintenance5PropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_budget (PhysicalDeviceMemoryBudgetPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_memory_decompression (PhysicalDeviceMemoryDecompressionFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_memory_decompression (PhysicalDeviceMemoryDecompressionPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (PhysicalDeviceMemoryPriorityFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceMemoryProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceMemoryProperties2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mesh_shader (PhysicalDeviceMeshShaderFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mesh_shader (PhysicalDeviceMeshShaderPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (PhysicalDeviceMultiDrawFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (PhysicalDeviceMultiDrawPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled (PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_multiview_per_view_attributes (PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas (PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_multiview_per_view_viewports (PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_mutable_descriptor_type (PhysicalDeviceMutableDescriptorTypeFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_nested_command_buffer (PhysicalDeviceNestedCommandBufferFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_nested_command_buffer (PhysicalDeviceNestedCommandBufferPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_non_seamless_cube_map (PhysicalDeviceNonSeamlessCubeMapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (PhysicalDeviceOpacityMicromapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (PhysicalDeviceOpacityMicromapPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (PhysicalDeviceOpticalFlowFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (PhysicalDeviceOpticalFlowPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pci_bus_info (PhysicalDevicePCIBusInfoPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pageable_device_local_memory (PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryPropertiesKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_cache_control (PhysicalDevicePipelineCreationCacheControlFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_library_group_handles (PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_properties (PhysicalDevicePipelinePropertiesFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_protected_access (PhysicalDevicePipelineProtectedAccessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_robustness (PhysicalDevicePipelineRobustnessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_robustness (PhysicalDevicePipelineRobustnessPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PhysicalDevicePointClippingProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_present_barrier (PhysicalDevicePresentBarrierFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_id (PhysicalDevicePresentIdFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_wait (PhysicalDevicePresentWaitFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_primitive_topology_list_restart (PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_primitives_generated_query (PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_private_data (PhysicalDevicePrivateDataFeatures)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceProperties2)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryFeatures)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (PhysicalDeviceProvokingVertexFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (PhysicalDeviceProvokingVertexPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_push_descriptor (PhysicalDevicePushDescriptorPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_rgba10x6_formats (PhysicalDeviceRGBA10X6FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access (PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_query (PhysicalDeviceRayQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder (PhysicalDeviceRayTracingInvocationReorderFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_invocation_reorder (PhysicalDeviceRayTracingInvocationReorderPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1 (PhysicalDeviceRayTracingMaintenance1FeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (PhysicalDeviceRayTracingMotionBlurFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelinePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_position_fetch (PhysicalDeviceRayTracingPositionFetchFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (PhysicalDeviceRayTracingPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_IMG_relaxed_line_rasterization (PhysicalDeviceRelaxedLineRasterizationFeaturesIMG)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_render_pass_striped (PhysicalDeviceRenderPassStripedFeaturesARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_render_pass_striped (PhysicalDeviceRenderPassStripedPropertiesARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2PropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (PhysicalDeviceSampleLocationsPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (PhysicalDeviceSamplerFilterMinmaxProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_scheduling_controls (PhysicalDeviceSchedulingControlsFeaturesARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_scheduling_controls (PhysicalDeviceSchedulingControlsPropertiesARM)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float2 (PhysicalDeviceShaderAtomicFloat2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64 (PhysicalDeviceShaderAtomicInt64Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_clock (PhysicalDeviceShaderClockFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_shader_core_builtins (PhysicalDeviceShaderCoreBuiltinsFeaturesARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_shader_core_builtins (PhysicalDeviceShaderCoreBuiltinsPropertiesARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_core_properties2 (PhysicalDeviceShaderCoreProperties2AMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_core_properties (PhysicalDeviceShaderCorePropertiesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_shader_core_properties (PhysicalDeviceShaderCorePropertiesARM)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_early_and_late_fragment_tests (PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (PhysicalDeviceShaderEnqueueFeaturesAMDX)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (PhysicalDeviceShaderEnqueuePropertiesAMDX)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductFeatures)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_shader_integer_dot_product (PhysicalDeviceShaderIntegerDotProductProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_module_identifier (PhysicalDeviceShaderModuleIdentifierFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_module_identifier (PhysicalDeviceShaderModuleIdentifierPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_object (PhysicalDeviceShaderObjectFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_object (PhysicalDeviceShaderObjectPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsPropertiesNV)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow (PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_shader_terminate_invocation (PhysicalDeviceShaderTerminateInvocationFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_tile_image (PhysicalDeviceShaderTileImageFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_tile_image (PhysicalDeviceShaderTileImagePropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImagePropertiesNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceSparseImageFormatInfo2)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceSparseProperties)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup (PhysicalDeviceSubgroupProperties)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlFeatures)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (PhysicalDeviceSubpassMergeFeedbackFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_subpass_shading (PhysicalDeviceSubpassShadingFeaturesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_subpass_shading (PhysicalDeviceSubpassShadingPropertiesHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (PhysicalDeviceSurfaceInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_swapchain_maintenance1 (PhysicalDeviceSwapchainMaintenance1FeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (PhysicalDeviceSynchronization2Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentProperties)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_texture_compression_astc_hdr (PhysicalDeviceTextureCompressionASTCHDRFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_tile_properties (PhysicalDeviceTilePropertiesFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreProperties)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_tooling_info (PhysicalDeviceToolProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (PhysicalDeviceVertexInputDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Properties)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Properties)
import {-# SOURCE #-} Vulkan.Core13 (PhysicalDeviceVulkan13Features)
import {-# SOURCE #-} Vulkan.Core13 (PhysicalDeviceVulkan13Properties)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_workgroup_memory_explicit_layout (PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats (PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_ycbcr_degamma (PhysicalDeviceYcbcrDegammaFeaturesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_image_arrays (PhysicalDeviceYcbcrImageArraysFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory (PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures)
import {-# SOURCE #-} Vulkan.Core10.PipelineCache (PipelineCacheCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (PipelineCacheHeaderVersionOne)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PipelineColorBlendAdvancedStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineColorBlendAttachmentState)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineColorBlendStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_color_write_enable (PipelineColorWriteCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_pipeline_compiler_control (PipelineCompilerControlCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (PipelineCoverageModulationStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PipelineCoverageReductionStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_coverage_to_color (PipelineCoverageToColorStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (PipelineCreateFlags2CreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedback)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineDepthStencilStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_discard_rectangles (PipelineDiscardRectangleStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineDynamicStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableInternalRepresentationKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutablePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableStatisticKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PipelineFragmentShadingRateEnumStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PipelineFragmentShadingRateStateCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands_compute (PipelineIndirectDeviceAddressInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineInputAssemblyStateCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PipelineLayoutCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineMultisampleStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_properties (PipelinePropertiesIdentifierEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (PipelineRasterizationConservativeStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PipelineRasterizationDepthClipStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PipelineRasterizationLineStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (PipelineRasterizationProvokingVertexStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineRasterizationStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_rasterization_order (PipelineRasterizationStateRasterizationOrderAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PipelineRasterizationStateStreamCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PipelineRenderingCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PipelineRepresentativeFragmentTestStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_robustness (PipelineRobustnessCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (PipelineSampleLocationsStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_module_identifier (PipelineShaderStageModuleIdentifierCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (PipelineShaderStageNodeCreateInfoAMDX)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PipelineTessellationDomainOriginStateCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineTessellationStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PipelineVertexInputDivisorStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineVertexInputStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportCoarseSampleOrderStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_control (PipelineViewportDepthClipControlCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PipelineViewportExclusiveScissorStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportShadingRateImageStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineViewportStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_viewport_swizzle (PipelineViewportSwizzleStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_clip_space_w_scaling (PipelineViewportWScalingStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_GGP_frame_token (PresentFrameTokenGGP)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_present_id (PresentIdKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (PresentInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (PresentRegionKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (PresentRegionsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PresentTimeGOOGLE)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PresentTimesInfoGOOGLE)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_private_data (PrivateDataSlotCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (ProtectedSubmitInfo)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PushConstantRange)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency (QueryLowLatencySupportNV)
import {-# SOURCE #-} Vulkan.Core10.Query (QueryPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (QueryPoolPerformanceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (QueryPoolPerformanceQueryCreateInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_synchronization2 (QueueFamilyCheckpointProperties2NV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints (QueueFamilyCheckpointPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_global_priority (QueueFamilyGlobalPriorityPropertiesKHR)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (QueueFamilyProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (QueueFamilyProperties2)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingPipelineCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (RayTracingPipelineCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingPipelineInterfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (RayTracingShaderGroupCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Rect2D)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (RectLayerKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (RefreshCycleDurationGOOGLE)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_swapchain_maintenance1 (ReleaseSwapchainImagesInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (RenderPassAttachmentBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (RenderPassBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (RenderPassCreateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (RenderPassCreateInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassCreationControlEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassCreationFeedbackCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassCreationFeedbackInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (RenderPassFragmentDensityMapCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (RenderPassInputAttachmentAspectCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (RenderPassMultiviewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (RenderPassSampleLocationsBeginInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_render_pass_striped (RenderPassStripeBeginInfoARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_render_pass_striped (RenderPassStripeInfoARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_render_pass_striped (RenderPassStripeSubmitInfoARM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassSubpassFeedbackCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subpass_merge_feedback (RenderPassSubpassFeedbackInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_render_pass_transform (RenderPassTransformBeginInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (RenderingAreaInfoKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingAttachmentInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (RenderingFragmentDensityMapAttachmentInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_dynamic_rendering (RenderingFragmentShadingRateAttachmentInfoKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ResolveImageInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing_motion_blur (SRTDataNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_image_processing2 (SamplerBlockMatchWindowCreateInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_border_color_swizzle (SamplerBorderColorComponentMappingCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (SamplerCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Sampler (SamplerCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_filter_cubic_weights (SamplerCubicWeightsCreateInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (SamplerCustomBorderColorCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (SamplerReductionModeCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_ycbcr_degamma (SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_external_memory_screen_buffer (ScreenBufferFormatPropertiesQNX)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_external_memory_screen_buffer (ScreenBufferPropertiesQNX)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_screen_surface (ScreenSurfaceCreateInfoQNX)
import {-# SOURCE #-} Vulkan.Core10.QueueSemaphore (SemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_fd (SemaphoreGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (SemaphoreGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_semaphore (SemaphoreGetZirconHandleInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreSignalInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SemaphoreSubmitInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreWaitInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (SetLatencyMarkerInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (SetStateFlagsIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_object (ShaderCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Shader (ShaderModuleCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_module_identifier (ShaderModuleIdentifierEXT)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (StridedDeviceAddressRegionKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import {-# SOURCE #-} Vulkan.Core10.Queue (SubmitInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SubmitInfo2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (SubpassDependency)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassDependency2)
import {-# SOURCE #-} Vulkan.Core10.Pass (SubpassDescription)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassDescription2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (SubpassDescriptionDepthStencilResolve)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassEndInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_fragment_density_map_offset (SubpassFragmentDensityMapOffsetEndInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled (SubpassResolvePerformanceQueryEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SubpassSampleLocationsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_subpass_shading (SubpassShadingPipelineCreateInfoHUAWEI)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (SubresourceHostMemcpySizeEXT)
import {-# SOURCE #-} Vulkan.Core10.Image (SubresourceLayout)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (SubresourceLayout2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCapabilities2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (SurfaceCapabilities2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceCapabilitiesFullScreenExclusiveEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_present_barrier (SurfaceCapabilitiesPresentBarrierNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (SurfaceFormat2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface (SurfaceFormatKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveWin32InfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_surface_maintenance1 (SurfacePresentModeCompatibilityEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_surface_maintenance1 (SurfacePresentModeEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_surface_maintenance1 (SurfacePresentScalingCapabilitiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface_protected_capabilities (SurfaceProtectedCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (SwapchainCounterCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_display_native_hdr (SwapchainDisplayNativeHdrCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (SwapchainLatencyCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_present_barrier (SwapchainPresentBarrierCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_swapchain_maintenance1 (SwapchainPresentFenceInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_swapchain_maintenance1 (SwapchainPresentModeInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_swapchain_maintenance1 (SwapchainPresentModesCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_swapchain_maintenance1 (SwapchainPresentScalingCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (SysmemColorSpaceFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_texture_gather_bias_lod (TextureLODGatherFormatPropertiesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_tile_properties (TilePropertiesQCOM)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1 (TraceRaysIndirectCommand2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (TraceRaysIndirectCommandKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (TransformMatrixKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_cache (ValidationCacheCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_features (ValidationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_flags (ValidationFlagsEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (VertexInputAttributeDescription)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (VertexInputAttributeDescription2EXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (VertexInputBindingDescription)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (VertexInputBindingDescription2EXT)
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
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (WriteDescriptorSetAccelerationStructureKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (WriteDescriptorSetAccelerationStructureNV)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (WriteDescriptorSetInlineUniformBlock)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_hdr_metadata (XYColorEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xcb_surface (XcbSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xlib_surface (XlibSurfaceCreateInfoKHR)
-- | VkBaseOutStructure - Base structure for a read-only pointer chain
--
-- = Description
--
-- 'BaseOutStructure' can be used to facilitate iterating through a
-- structure pointer chain that returns data back to the application.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'BaseOutStructure', 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_EXT_pipeline_properties.getPipelinePropertiesEXT'
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BaseOutStructure{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (sType)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseOutStructure))) (next)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BaseInStructure{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (sType)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseInStructure))) (next)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
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
  Extends AccelerationStructureCreateInfoKHR OpaqueCaptureDescriptorDataCreateInfoEXT = ()
  Extends AccelerationStructureCreateInfoKHR AccelerationStructureMotionInfoNV = ()
  Extends AccelerationStructureCreateInfoNV OpaqueCaptureDescriptorDataCreateInfoEXT = ()
  Extends AccelerationStructureGeometryTrianglesDataKHR AccelerationStructureGeometryMotionTrianglesDataNV = ()
  Extends AccelerationStructureGeometryTrianglesDataKHR AccelerationStructureTrianglesOpacityMicromapEXT = ()
  Extends AccelerationStructureGeometryTrianglesDataKHR AccelerationStructureTrianglesDisplacementMicromapNV = ()
  Extends AndroidHardwareBufferPropertiesANDROID AndroidHardwareBufferFormatPropertiesANDROID = ()
  Extends AndroidHardwareBufferPropertiesANDROID AndroidHardwareBufferFormatProperties2ANDROID = ()
  Extends AndroidHardwareBufferPropertiesANDROID AndroidHardwareBufferFormatResolvePropertiesANDROID = ()
  Extends AttachmentDescription2 ExternalFormatANDROID = ()
  Extends AttachmentDescription2 AttachmentDescriptionStencilLayout = ()
  Extends AttachmentReference2 AttachmentReferenceStencilLayout = ()
  Extends BindBufferMemoryInfo BindBufferMemoryDeviceGroupInfo = ()
  Extends BindImageMemoryInfo BindImageMemoryDeviceGroupInfo = ()
  Extends BindImageMemoryInfo BindImageMemorySwapchainInfoKHR = ()
  Extends BindImageMemoryInfo BindImagePlaneMemoryInfo = ()
  Extends BindSparseInfo DeviceGroupBindSparseInfo = ()
  Extends BindSparseInfo TimelineSemaphoreSubmitInfo = ()
  Extends BindSparseInfo FrameBoundaryEXT = ()
  Extends BlitImageInfo2 BlitImageCubicWeightsInfoQCOM = ()
  Extends BufferCreateInfo BufferUsageFlags2CreateInfoKHR = ()
  Extends BufferCreateInfo DedicatedAllocationBufferCreateInfoNV = ()
  Extends BufferCreateInfo ExternalMemoryBufferCreateInfo = ()
  Extends BufferCreateInfo BufferOpaqueCaptureAddressCreateInfo = ()
  Extends BufferCreateInfo BufferDeviceAddressCreateInfoEXT = ()
  Extends BufferCreateInfo OpaqueCaptureDescriptorDataCreateInfoEXT = ()
  Extends BufferCreateInfo BufferCollectionBufferCreateInfoFUCHSIA = ()
  Extends BufferImageCopy2 CopyCommandTransformInfoQCOM = ()
  Extends BufferMemoryBarrier ExternalMemoryAcquireUnmodifiedEXT = ()
  Extends BufferMemoryBarrier2 ExternalMemoryAcquireUnmodifiedEXT = ()
  Extends BufferViewCreateInfo BufferUsageFlags2CreateInfoKHR = ()
  Extends BufferViewCreateInfo ExportMetalObjectCreateInfoEXT = ()
  Extends CommandBufferBeginInfo DeviceGroupCommandBufferBeginInfo = ()
  Extends CommandBufferInheritanceInfo CommandBufferInheritanceConditionalRenderingInfoEXT = ()
  Extends CommandBufferInheritanceInfo ExternalFormatANDROID = ()
  Extends CommandBufferInheritanceInfo CommandBufferInheritanceRenderPassTransformInfoQCOM = ()
  Extends CommandBufferInheritanceInfo CommandBufferInheritanceViewportScissorInfoNV = ()
  Extends CommandBufferInheritanceInfo CommandBufferInheritanceRenderingInfo = ()
  Extends CommandBufferInheritanceInfo AttachmentSampleCountInfoAMD = ()
  Extends CommandBufferInheritanceInfo MultiviewPerViewAttributesInfoNVX = ()
  Extends CommandBufferSubmitInfo RenderPassStripeSubmitInfoARM = ()
  Extends ComputePipelineCreateInfo PipelineCreateFlags2CreateInfoKHR = ()
  Extends ComputePipelineCreateInfo PipelineCreationFeedbackCreateInfo = ()
  Extends ComputePipelineCreateInfo SubpassShadingPipelineCreateInfoHUAWEI = ()
  Extends ComputePipelineCreateInfo PipelineCompilerControlCreateInfoAMD = ()
  Extends ComputePipelineCreateInfo PipelineRobustnessCreateInfoEXT = ()
  Extends DebugUtilsMessengerCallbackDataEXT DeviceAddressBindingCallbackDataEXT = ()
  Extends DepthBiasInfoEXT DepthBiasRepresentationInfoEXT = ()
  Extends DescriptorBufferBindingInfoEXT BufferUsageFlags2CreateInfoKHR = ()
  Extends DescriptorBufferBindingInfoEXT DescriptorBufferBindingPushDescriptorBufferHandleEXT = ()
  Extends DescriptorPoolCreateInfo DescriptorPoolInlineUniformBlockCreateInfo = ()
  Extends DescriptorPoolCreateInfo MutableDescriptorTypeCreateInfoEXT = ()
  Extends DescriptorSetAllocateInfo DescriptorSetVariableDescriptorCountAllocateInfo = ()
  Extends DescriptorSetLayoutCreateInfo DescriptorSetLayoutBindingFlagsCreateInfo = ()
  Extends DescriptorSetLayoutCreateInfo MutableDescriptorTypeCreateInfoEXT = ()
  Extends DescriptorSetLayoutSupport DescriptorSetVariableDescriptorCountLayoutSupport = ()
  Extends DeviceCreateInfo PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV = ()
  Extends DeviceCreateInfo DevicePrivateDataCreateInfo = ()
  Extends DeviceCreateInfo PhysicalDevicePrivateDataFeatures = ()
  Extends DeviceCreateInfo (PhysicalDeviceFeatures2 '[]) = ()
  Extends DeviceCreateInfo PhysicalDeviceVariablePointersFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceMultiviewFeatures = ()
  Extends DeviceCreateInfo DeviceGroupDeviceCreateInfo = ()
  Extends DeviceCreateInfo PhysicalDevicePresentIdFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDevicePresentWaitFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDevice16BitStorageFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderSubgroupExtendedTypesFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceSamplerYcbcrConversionFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceProtectedMemoryFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceBlendOperationAdvancedFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceMultiDrawFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceInlineUniformBlockFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceMaintenance4Features = ()
  Extends DeviceCreateInfo PhysicalDeviceMaintenance5FeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderDrawParametersFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderFloat16Int8Features = ()
  Extends DeviceCreateInfo PhysicalDeviceHostQueryResetFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceGlobalPriorityQueryFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceDeviceMemoryReportFeaturesEXT = ()
  Extends DeviceCreateInfo DeviceDeviceMemoryReportCreateInfoEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDescriptorIndexingFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceTimelineSemaphoreFeatures = ()
  Extends DeviceCreateInfo PhysicalDevice8BitStorageFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceConditionalRenderingFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceVulkanMemoryModelFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderAtomicInt64Features = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderAtomicFloatFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderAtomicFloat2FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceVertexAttributeDivisorFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceASTCDecodeFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceTransformFeedbackFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceRepresentativeFragmentTestFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceExclusiveScissorFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceCornerSampledImageFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceComputeShaderDerivativesFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderImageFootprintFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceCopyMemoryIndirectFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceMemoryDecompressionFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceShadingRateImageFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceInvocationMaskFeaturesHUAWEI = ()
  Extends DeviceCreateInfo PhysicalDeviceMeshShaderFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceMeshShaderFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceAccelerationStructureFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceRayTracingPipelineFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceRayQueryFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceRayTracingMaintenance1FeaturesKHR = ()
  Extends DeviceCreateInfo DeviceMemoryOverallocationCreateInfoAMD = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentDensityMapFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentDensityMap2FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceScalarBlockLayoutFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceUniformBufferStandardLayoutFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceDepthClipEnableFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceMemoryPriorityFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceBufferDeviceAddressFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceBufferDeviceAddressFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImagelessFramebufferFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceTextureCompressionASTCHDRFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceCooperativeMatrixFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceYcbcrImageArraysFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePresentBarrierFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDevicePerformanceQueryFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceCoverageReductionModeFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderClockFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceIndexTypeUint8FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderSMBuiltinsFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentShaderInterlockFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSeparateDepthStencilLayoutsFeatures = ()
  Extends DeviceCreateInfo PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderDemoteToHelperInvocationFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceTexelBufferAlignmentFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSubgroupSizeControlFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceLineRasterizationFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePipelineCreationCacheControlFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceVulkan11Features = ()
  Extends DeviceCreateInfo PhysicalDeviceVulkan12Features = ()
  Extends DeviceCreateInfo PhysicalDeviceVulkan13Features = ()
  Extends DeviceCreateInfo PhysicalDeviceCoherentMemoryFeaturesAMD = ()
  Extends DeviceCreateInfo PhysicalDeviceCustomBorderColorFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceBorderColorSwizzleFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceExtendedDynamicStateFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceExtendedDynamicState2FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceExtendedDynamicState3FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDiagnosticsConfigFeaturesNV = ()
  Extends DeviceCreateInfo DeviceDiagnosticsConfigCreateInfoNV = ()
  Extends DeviceCreateInfo PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceRobustness2FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImageRobustnessFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDevicePortabilitySubsetFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDevice4444FormatsFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSubpassShadingFeaturesHUAWEI = ()
  Extends DeviceCreateInfo (PhysicalDeviceClusterCullingShaderFeaturesHUAWEI '[]) = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentShadingRateFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderTerminateInvocationFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceImage2DViewOf3DFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImageSlicedViewOf3DFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceMutableDescriptorTypeFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDepthClipControlFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceVertexInputDynamicStateFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceExternalMemoryRDMAFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceColorWriteEnableFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSynchronization2Features = ()
  Extends DeviceCreateInfo PhysicalDeviceHostImageCopyFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceLegacyDitheringFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePipelineProtectedAccessFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceInheritedViewportScissorFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceProvokingVertexFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDescriptorBufferFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderIntegerDotProductFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentShaderBarycentricFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceRayTracingMotionBlurFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceRGBA10X6FormatsFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDynamicRenderingFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceImageViewMinLodFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceLinearColorAttachmentFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE = ()
  Extends DeviceCreateInfo PhysicalDeviceNestedCommandBufferFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderModuleIdentifierFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImageCompressionControlFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSubpassMergeFeedbackFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceOpacityMicromapFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDisplacementMicromapFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDevicePipelinePropertiesFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD = ()
  Extends DeviceCreateInfo PhysicalDeviceNonSeamlessCubeMapFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePipelineRobustnessFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImageProcessingFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceTilePropertiesFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceAmigoProfilingFeaturesSEC = ()
  Extends DeviceCreateInfo PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDepthClampZeroOneFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceAddressBindingReportFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceOpticalFlowFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceFaultFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderCoreBuiltinsFeaturesARM = ()
  Extends DeviceCreateInfo PhysicalDeviceFrameBoundaryFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSwapchainMaintenance1FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDepthBiasControlFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceRayTracingInvocationReorderFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceRayTracingPositionFetchFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderObjectFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderTileImageFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX = ()
  Extends DeviceCreateInfo PhysicalDeviceCooperativeMatrixFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderEnqueueFeaturesAMDX = ()
  Extends DeviceCreateInfo PhysicalDeviceCubicClampFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceYcbcrDegammaFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceCubicWeightsFeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceImageProcessing2FeaturesQCOM = ()
  Extends DeviceCreateInfo PhysicalDeviceDescriptorPoolOverallocationFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceExternalFormatResolveFeaturesANDROID = ()
  Extends DeviceCreateInfo PhysicalDeviceCudaKernelLaunchFeaturesNV = ()
  Extends DeviceCreateInfo DeviceQueueShaderCoreControlCreateInfoARM = ()
  Extends DeviceCreateInfo PhysicalDeviceSchedulingControlsFeaturesARM = ()
  Extends DeviceCreateInfo PhysicalDeviceRelaxedLineRasterizationFeaturesIMG = ()
  Extends DeviceCreateInfo PhysicalDeviceRenderPassStripedFeaturesARM = ()
  Extends DeviceQueueCreateInfo DeviceQueueGlobalPriorityCreateInfoKHR = ()
  Extends DeviceQueueCreateInfo DeviceQueueShaderCoreControlCreateInfoARM = ()
  Extends EventCreateInfo ExportMetalObjectCreateInfoEXT = ()
  Extends EventCreateInfo ImportMetalSharedEventInfoEXT = ()
  Extends ExecutionGraphPipelineCreateInfoAMDX PipelineCreationFeedbackCreateInfo = ()
  Extends ExecutionGraphPipelineCreateInfoAMDX PipelineCompilerControlCreateInfoAMD = ()
  Extends ExportMetalObjectsInfoEXT ExportMetalDeviceInfoEXT = ()
  Extends ExportMetalObjectsInfoEXT ExportMetalCommandQueueInfoEXT = ()
  Extends ExportMetalObjectsInfoEXT ExportMetalBufferInfoEXT = ()
  Extends ExportMetalObjectsInfoEXT ExportMetalTextureInfoEXT = ()
  Extends ExportMetalObjectsInfoEXT ExportMetalIOSurfaceInfoEXT = ()
  Extends ExportMetalObjectsInfoEXT ExportMetalSharedEventInfoEXT = ()
  Extends FenceCreateInfo ExportFenceCreateInfo = ()
  Extends FenceCreateInfo ExportFenceWin32HandleInfoKHR = ()
  Extends FormatProperties2 DrmFormatModifierPropertiesListEXT = ()
  Extends FormatProperties2 SubpassResolvePerformanceQueryEXT = ()
  Extends FormatProperties2 FormatProperties3 = ()
  Extends FormatProperties2 DrmFormatModifierPropertiesList2EXT = ()
  Extends FramebufferCreateInfo FramebufferAttachmentsCreateInfo = ()
  Extends GraphicsPipelineCreateInfo PipelineCreateFlags2CreateInfoKHR = ()
  Extends GraphicsPipelineCreateInfo GraphicsPipelineShaderGroupsCreateInfoNV = ()
  Extends GraphicsPipelineCreateInfo PipelineDiscardRectangleStateCreateInfoEXT = ()
  Extends GraphicsPipelineCreateInfo ExternalFormatANDROID = ()
  Extends GraphicsPipelineCreateInfo PipelineRepresentativeFragmentTestStateCreateInfoNV = ()
  Extends GraphicsPipelineCreateInfo PipelineCreationFeedbackCreateInfo = ()
  Extends GraphicsPipelineCreateInfo PipelineCompilerControlCreateInfoAMD = ()
  Extends GraphicsPipelineCreateInfo PipelineLibraryCreateInfoKHR = ()
  Extends GraphicsPipelineCreateInfo PipelineFragmentShadingRateStateCreateInfoKHR = ()
  Extends GraphicsPipelineCreateInfo PipelineFragmentShadingRateEnumStateCreateInfoNV = ()
  Extends GraphicsPipelineCreateInfo PipelineRenderingCreateInfo = ()
  Extends GraphicsPipelineCreateInfo AttachmentSampleCountInfoAMD = ()
  Extends GraphicsPipelineCreateInfo MultiviewPerViewAttributesInfoNVX = ()
  Extends GraphicsPipelineCreateInfo GraphicsPipelineLibraryCreateInfoEXT = ()
  Extends GraphicsPipelineCreateInfo PipelineRobustnessCreateInfoEXT = ()
  Extends ImageBlit2 CopyCommandTransformInfoQCOM = ()
  Extends ImageCreateInfo DedicatedAllocationImageCreateInfoNV = ()
  Extends ImageCreateInfo ExternalMemoryImageCreateInfoNV = ()
  Extends ImageCreateInfo ExternalMemoryImageCreateInfo = ()
  Extends ImageCreateInfo ImageSwapchainCreateInfoKHR = ()
  Extends ImageCreateInfo ImageFormatListCreateInfo = ()
  Extends ImageCreateInfo ExternalFormatANDROID = ()
  Extends ImageCreateInfo ImageDrmFormatModifierListCreateInfoEXT = ()
  Extends ImageCreateInfo ImageDrmFormatModifierExplicitCreateInfoEXT = ()
  Extends ImageCreateInfo ImageStencilUsageCreateInfo = ()
  Extends ImageCreateInfo OpaqueCaptureDescriptorDataCreateInfoEXT = ()
  Extends ImageCreateInfo BufferCollectionImageCreateInfoFUCHSIA = ()
  Extends ImageCreateInfo ImageCompressionControlEXT = ()
  Extends ImageCreateInfo ExportMetalObjectCreateInfoEXT = ()
  Extends ImageCreateInfo ImportMetalTextureInfoEXT = ()
  Extends ImageCreateInfo ImportMetalIOSurfaceInfoEXT = ()
  Extends ImageCreateInfo OpticalFlowImageFormatInfoNV = ()
  Extends ImageCreateInfo ExternalFormatQNX = ()
  Extends ImageFormatProperties2 ExternalImageFormatProperties = ()
  Extends ImageFormatProperties2 SamplerYcbcrConversionImageFormatProperties = ()
  Extends ImageFormatProperties2 TextureLODGatherFormatPropertiesAMD = ()
  Extends ImageFormatProperties2 AndroidHardwareBufferUsageANDROID = ()
  Extends ImageFormatProperties2 FilterCubicImageViewImageFormatPropertiesEXT = ()
  Extends ImageFormatProperties2 HostImageCopyDevicePerformanceQueryEXT = ()
  Extends ImageFormatProperties2 ImageCompressionPropertiesEXT = ()
  Extends ImageMemoryBarrier SampleLocationsInfoEXT = ()
  Extends ImageMemoryBarrier ExternalMemoryAcquireUnmodifiedEXT = ()
  Extends ImageMemoryBarrier2 SampleLocationsInfoEXT = ()
  Extends ImageMemoryBarrier2 ExternalMemoryAcquireUnmodifiedEXT = ()
  Extends ImageMemoryRequirementsInfo2 ImagePlaneMemoryRequirementsInfo = ()
  Extends ImageViewCreateInfo ImageViewUsageCreateInfo = ()
  Extends ImageViewCreateInfo ImageViewSlicedCreateInfoEXT = ()
  Extends ImageViewCreateInfo SamplerYcbcrConversionInfo = ()
  Extends ImageViewCreateInfo ImageViewASTCDecodeModeEXT = ()
  Extends ImageViewCreateInfo OpaqueCaptureDescriptorDataCreateInfoEXT = ()
  Extends ImageViewCreateInfo ImageViewMinLodCreateInfoEXT = ()
  Extends ImageViewCreateInfo ExportMetalObjectCreateInfoEXT = ()
  Extends ImageViewCreateInfo ImageViewSampleWeightCreateInfoQCOM = ()
  Extends InstanceCreateInfo DebugReportCallbackCreateInfoEXT = ()
  Extends InstanceCreateInfo ValidationFlagsEXT = ()
  Extends InstanceCreateInfo ValidationFeaturesEXT = ()
  Extends InstanceCreateInfo LayerSettingsCreateInfoEXT = ()
  Extends InstanceCreateInfo DebugUtilsMessengerCreateInfoEXT = ()
  Extends InstanceCreateInfo ExportMetalObjectCreateInfoEXT = ()
  Extends InstanceCreateInfo DirectDriverLoadingListLUNARG = ()
  Extends MemoryAllocateInfo DedicatedAllocationMemoryAllocateInfoNV = ()
  Extends MemoryAllocateInfo ExportMemoryAllocateInfoNV = ()
  Extends MemoryAllocateInfo ImportMemoryWin32HandleInfoNV = ()
  Extends MemoryAllocateInfo ExportMemoryWin32HandleInfoNV = ()
  Extends MemoryAllocateInfo ExportMemoryAllocateInfo = ()
  Extends MemoryAllocateInfo ImportMemoryWin32HandleInfoKHR = ()
  Extends MemoryAllocateInfo ExportMemoryWin32HandleInfoKHR = ()
  Extends MemoryAllocateInfo ImportMemoryZirconHandleInfoFUCHSIA = ()
  Extends MemoryAllocateInfo ImportMemoryFdInfoKHR = ()
  Extends MemoryAllocateInfo MemoryAllocateFlagsInfo = ()
  Extends MemoryAllocateInfo MemoryDedicatedAllocateInfo = ()
  Extends MemoryAllocateInfo ImportMemoryHostPointerInfoEXT = ()
  Extends MemoryAllocateInfo ImportAndroidHardwareBufferInfoANDROID = ()
  Extends MemoryAllocateInfo MemoryPriorityAllocateInfoEXT = ()
  Extends MemoryAllocateInfo MemoryOpaqueCaptureAddressAllocateInfo = ()
  Extends MemoryAllocateInfo ImportMemoryBufferCollectionFUCHSIA = ()
  Extends MemoryAllocateInfo ExportMetalObjectCreateInfoEXT = ()
  Extends MemoryAllocateInfo ImportMetalBufferInfoEXT = ()
  Extends MemoryAllocateInfo ImportScreenBufferInfoQNX = ()
  Extends MemoryRequirements2 MemoryDedicatedRequirements = ()
  Extends OpticalFlowSessionCreateInfoNV OpticalFlowSessionCreatePrivateDataInfoNV = ()
  Extends PhysicalDeviceClusterCullingShaderFeaturesHUAWEI PhysicalDeviceClusterCullingShaderVrsFeaturesHUAWEI = ()
  Extends PhysicalDeviceExternalBufferInfo BufferUsageFlags2CreateInfoKHR = ()
  Extends PhysicalDeviceExternalSemaphoreInfo SemaphoreTypeCreateInfo = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePrivateDataFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVariablePointersFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMultiviewFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePresentIdFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePresentWaitFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevice16BitStorageFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderSubgroupExtendedTypesFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSamplerYcbcrConversionFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceProtectedMemoryFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceBlendOperationAdvancedFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMultiDrawFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceInlineUniformBlockFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMaintenance4Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMaintenance5FeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderDrawParametersFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderFloat16Int8Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceHostQueryResetFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceGlobalPriorityQueryFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDeviceMemoryReportFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDescriptorIndexingFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTimelineSemaphoreFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevice8BitStorageFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceConditionalRenderingFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVulkanMemoryModelFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderAtomicInt64Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderAtomicFloatFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderAtomicFloat2FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVertexAttributeDivisorFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceASTCDecodeFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTransformFeedbackFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRepresentativeFragmentTestFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExclusiveScissorFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCornerSampledImageFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceComputeShaderDerivativesFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderImageFootprintFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCopyMemoryIndirectFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMemoryDecompressionFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShadingRateImageFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceInvocationMaskFeaturesHUAWEI = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMeshShaderFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMeshShaderFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceAccelerationStructureFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRayTracingPipelineFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRayQueryFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRayTracingMaintenance1FeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentDensityMapFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentDensityMap2FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceScalarBlockLayoutFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceUniformBufferStandardLayoutFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDepthClipEnableFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMemoryPriorityFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceBufferDeviceAddressFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceBufferDeviceAddressFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImagelessFramebufferFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTextureCompressionASTCHDRFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCooperativeMatrixFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceYcbcrImageArraysFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePresentBarrierFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePerformanceQueryFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCoverageReductionModeFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderClockFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceIndexTypeUint8FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderSMBuiltinsFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentShaderInterlockFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSeparateDepthStencilLayoutsFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderDemoteToHelperInvocationFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTexelBufferAlignmentFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSubgroupSizeControlFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceLineRasterizationFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelineCreationCacheControlFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVulkan11Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVulkan12Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVulkan13Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCoherentMemoryFeaturesAMD = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCustomBorderColorFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceBorderColorSwizzleFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExtendedDynamicStateFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExtendedDynamicState2FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExtendedDynamicState3FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDiagnosticsConfigFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRobustness2FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageRobustnessFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePortabilitySubsetFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevice4444FormatsFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSubpassShadingFeaturesHUAWEI = ()
  Extends PhysicalDeviceFeatures2 (PhysicalDeviceClusterCullingShaderFeaturesHUAWEI '[]) = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentShadingRateFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderTerminateInvocationFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImage2DViewOf3DFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageSlicedViewOf3DFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMutableDescriptorTypeFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDepthClipControlFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVertexInputDynamicStateFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExternalMemoryRDMAFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceColorWriteEnableFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSynchronization2Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceHostImageCopyFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceLegacyDitheringFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelineProtectedAccessFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceInheritedViewportScissorFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceProvokingVertexFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDescriptorBufferFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderIntegerDotProductFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentShaderBarycentricFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRayTracingMotionBlurFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRGBA10X6FormatsFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDynamicRenderingFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageViewMinLodFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceLinearColorAttachmentFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceNestedCommandBufferFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderModuleIdentifierFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageCompressionControlFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSubpassMergeFeedbackFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceOpacityMicromapFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDisplacementMicromapFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelinePropertiesFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceNonSeamlessCubeMapFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelineRobustnessFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageProcessingFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTilePropertiesFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceAmigoProfilingFeaturesSEC = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDepthClampZeroOneFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceAddressBindingReportFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceOpticalFlowFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFaultFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderCoreBuiltinsFeaturesARM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFrameBoundaryFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSwapchainMaintenance1FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDepthBiasControlFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRayTracingInvocationReorderFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRayTracingPositionFetchFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderObjectFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderTileImageFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCooperativeMatrixFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderEnqueueFeaturesAMDX = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCubicClampFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceYcbcrDegammaFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCubicWeightsFeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageProcessing2FeaturesQCOM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDescriptorPoolOverallocationFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExternalFormatResolveFeaturesANDROID = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCudaKernelLaunchFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSchedulingControlsFeaturesARM = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRelaxedLineRasterizationFeaturesIMG = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRenderPassStripedFeaturesARM = ()
  Extends PhysicalDeviceImageFormatInfo2 PhysicalDeviceExternalImageFormatInfo = ()
  Extends PhysicalDeviceImageFormatInfo2 ImageFormatListCreateInfo = ()
  Extends PhysicalDeviceImageFormatInfo2 PhysicalDeviceImageDrmFormatModifierInfoEXT = ()
  Extends PhysicalDeviceImageFormatInfo2 ImageStencilUsageCreateInfo = ()
  Extends PhysicalDeviceImageFormatInfo2 PhysicalDeviceImageViewImageFormatInfoEXT = ()
  Extends PhysicalDeviceImageFormatInfo2 ImageCompressionControlEXT = ()
  Extends PhysicalDeviceImageFormatInfo2 OpticalFlowImageFormatInfoNV = ()
  Extends PhysicalDeviceMemoryProperties2 PhysicalDeviceMemoryBudgetPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDeviceGeneratedCommandsPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMultiDrawPropertiesEXT = ()
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
  Extends PhysicalDeviceProperties2 PhysicalDeviceInlineUniformBlockProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMaintenance3Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMaintenance4Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMaintenance5PropertiesKHR = ()
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
  Extends PhysicalDeviceProperties2 PhysicalDeviceCopyMemoryIndirectPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMemoryDecompressionPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShadingRateImagePropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMeshShaderPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMeshShaderPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceAccelerationStructurePropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRayTracingPipelinePropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRayTracingPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentDensityMapPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentDensityMap2PropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceCooperativeMatrixPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePerformanceQueryPropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderSMBuiltinsPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceTexelBufferAlignmentProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceSubgroupSizeControlProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceSubpassShadingPropertiesHUAWEI = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceClusterCullingShaderPropertiesHUAWEI = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceLineRasterizationPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceVulkan11Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceVulkan12Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceVulkan13Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceCustomBorderColorPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceExtendedDynamicState3PropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRobustness2PropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePortabilitySubsetPropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentShadingRatePropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentShadingRateEnumsPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceHostImageCopyPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceProvokingVertexPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDescriptorBufferPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderIntegerDotProductProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDrmPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentShaderBarycentricPropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceNestedCommandBufferPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderModuleIdentifierPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceOpacityMicromapPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDisplacementMicromapPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePipelineRobustnessPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceImageProcessingPropertiesQCOM = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceOpticalFlowPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderCoreBuiltinsPropertiesARM = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRayTracingInvocationReorderPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceExtendedSparseAddressSpacePropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderCorePropertiesARM = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderObjectPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderTileImagePropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceCooperativeMatrixPropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderEnqueuePropertiesAMDX = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceImageProcessing2PropertiesQCOM = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceLayeredDriverPropertiesMSFT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceExternalFormatResolvePropertiesANDROID = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceCudaKernelLaunchPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceSchedulingControlsPropertiesARM = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRenderPassStripedPropertiesARM = ()
  Extends PhysicalDeviceSurfaceInfo2KHR SurfaceFullScreenExclusiveInfoEXT = ()
  Extends PhysicalDeviceSurfaceInfo2KHR SurfaceFullScreenExclusiveWin32InfoEXT = ()
  Extends PhysicalDeviceSurfaceInfo2KHR SurfacePresentModeEXT = ()
  Extends PipelineColorBlendStateCreateInfo PipelineColorBlendAdvancedStateCreateInfoEXT = ()
  Extends PipelineColorBlendStateCreateInfo PipelineColorWriteCreateInfoEXT = ()
  Extends PipelineMultisampleStateCreateInfo PipelineCoverageToColorStateCreateInfoNV = ()
  Extends PipelineMultisampleStateCreateInfo PipelineSampleLocationsStateCreateInfoEXT = ()
  Extends PipelineMultisampleStateCreateInfo PipelineCoverageModulationStateCreateInfoNV = ()
  Extends PipelineMultisampleStateCreateInfo PipelineCoverageReductionStateCreateInfoNV = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationStateRasterizationOrderAMD = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationConservativeStateCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationStateStreamCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationDepthClipStateCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationLineStateCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationProvokingVertexStateCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo DepthBiasRepresentationInfoEXT = ()
  Extends PipelineShaderStageCreateInfo (ShaderModuleCreateInfo '[]) = ()
  Extends PipelineShaderStageCreateInfo ShaderModuleValidationCacheCreateInfoEXT = ()
  Extends PipelineShaderStageCreateInfo DebugUtilsObjectNameInfoEXT = ()
  Extends PipelineShaderStageCreateInfo PipelineShaderStageRequiredSubgroupSizeCreateInfo = ()
  Extends PipelineShaderStageCreateInfo PipelineShaderStageModuleIdentifierCreateInfoEXT = ()
  Extends PipelineShaderStageCreateInfo PipelineRobustnessCreateInfoEXT = ()
  Extends PipelineShaderStageCreateInfo PipelineShaderStageNodeCreateInfoAMDX = ()
  Extends PipelineTessellationStateCreateInfo PipelineTessellationDomainOriginStateCreateInfo = ()
  Extends PipelineVertexInputStateCreateInfo PipelineVertexInputDivisorStateCreateInfoEXT = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportWScalingStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportSwizzleStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportExclusiveScissorStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportShadingRateImageStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportCoarseSampleOrderStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportDepthClipControlCreateInfoEXT = ()
  Extends PresentInfoKHR DisplayPresentInfoKHR = ()
  Extends PresentInfoKHR PresentRegionsKHR = ()
  Extends PresentInfoKHR DeviceGroupPresentInfoKHR = ()
  Extends PresentInfoKHR PresentIdKHR = ()
  Extends PresentInfoKHR PresentTimesInfoGOOGLE = ()
  Extends PresentInfoKHR PresentFrameTokenGGP = ()
  Extends PresentInfoKHR FrameBoundaryEXT = ()
  Extends PresentInfoKHR SwapchainPresentFenceInfoEXT = ()
  Extends PresentInfoKHR SwapchainPresentModeInfoEXT = ()
  Extends QueryPoolCreateInfo QueryPoolPerformanceCreateInfoKHR = ()
  Extends QueryPoolCreateInfo QueryPoolPerformanceQueryCreateInfoINTEL = ()
  Extends QueueFamilyProperties2 QueueFamilyGlobalPriorityPropertiesKHR = ()
  Extends QueueFamilyProperties2 QueueFamilyCheckpointPropertiesNV = ()
  Extends QueueFamilyProperties2 QueueFamilyCheckpointProperties2NV = ()
  Extends RayTracingPipelineCreateInfoKHR PipelineCreateFlags2CreateInfoKHR = ()
  Extends RayTracingPipelineCreateInfoKHR PipelineCreationFeedbackCreateInfo = ()
  Extends RayTracingPipelineCreateInfoKHR PipelineRobustnessCreateInfoEXT = ()
  Extends RayTracingPipelineCreateInfoNV PipelineCreateFlags2CreateInfoKHR = ()
  Extends RayTracingPipelineCreateInfoNV PipelineCreationFeedbackCreateInfo = ()
  Extends RenderPassBeginInfo DeviceGroupRenderPassBeginInfo = ()
  Extends RenderPassBeginInfo RenderPassSampleLocationsBeginInfoEXT = ()
  Extends RenderPassBeginInfo RenderPassAttachmentBeginInfo = ()
  Extends RenderPassBeginInfo RenderPassTransformBeginInfoQCOM = ()
  Extends RenderPassBeginInfo MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM = ()
  Extends RenderPassBeginInfo RenderPassStripeBeginInfoARM = ()
  Extends RenderPassCreateInfo RenderPassMultiviewCreateInfo = ()
  Extends RenderPassCreateInfo RenderPassInputAttachmentAspectCreateInfo = ()
  Extends RenderPassCreateInfo RenderPassFragmentDensityMapCreateInfoEXT = ()
  Extends RenderPassCreateInfo2 RenderPassFragmentDensityMapCreateInfoEXT = ()
  Extends RenderPassCreateInfo2 RenderPassCreationControlEXT = ()
  Extends RenderPassCreateInfo2 RenderPassCreationFeedbackCreateInfoEXT = ()
  Extends RenderingInfo DeviceGroupRenderPassBeginInfo = ()
  Extends RenderingInfo MultisampledRenderToSingleSampledInfoEXT = ()
  Extends RenderingInfo RenderingFragmentShadingRateAttachmentInfoKHR = ()
  Extends RenderingInfo RenderingFragmentDensityMapAttachmentInfoEXT = ()
  Extends RenderingInfo MultiviewPerViewAttributesInfoNVX = ()
  Extends RenderingInfo MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM = ()
  Extends RenderingInfo RenderPassStripeBeginInfoARM = ()
  Extends SamplerCreateInfo SamplerYcbcrConversionInfo = ()
  Extends SamplerCreateInfo SamplerReductionModeCreateInfo = ()
  Extends SamplerCreateInfo SamplerCustomBorderColorCreateInfoEXT = ()
  Extends SamplerCreateInfo SamplerBorderColorComponentMappingCreateInfoEXT = ()
  Extends SamplerCreateInfo OpaqueCaptureDescriptorDataCreateInfoEXT = ()
  Extends SamplerCreateInfo SamplerCubicWeightsCreateInfoQCOM = ()
  Extends SamplerCreateInfo SamplerBlockMatchWindowCreateInfoQCOM = ()
  Extends SamplerYcbcrConversionCreateInfo ExternalFormatANDROID = ()
  Extends SamplerYcbcrConversionCreateInfo ExternalFormatQNX = ()
  Extends SamplerYcbcrConversionCreateInfo SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM = ()
  Extends ScreenBufferPropertiesQNX ScreenBufferFormatPropertiesQNX = ()
  Extends SemaphoreCreateInfo ExportSemaphoreCreateInfo = ()
  Extends SemaphoreCreateInfo ExportSemaphoreWin32HandleInfoKHR = ()
  Extends SemaphoreCreateInfo SemaphoreTypeCreateInfo = ()
  Extends SemaphoreCreateInfo ExportMetalObjectCreateInfoEXT = ()
  Extends SemaphoreCreateInfo ImportMetalSharedEventInfoEXT = ()
  Extends SemaphoreCreateInfo QueryLowLatencySupportNV = ()
  Extends ShaderCreateInfoEXT PipelineShaderStageRequiredSubgroupSizeCreateInfo = ()
  Extends ShaderModuleCreateInfo ShaderModuleValidationCacheCreateInfoEXT = ()
  Extends SubmitInfo Win32KeyedMutexAcquireReleaseInfoNV = ()
  Extends SubmitInfo Win32KeyedMutexAcquireReleaseInfoKHR = ()
  Extends SubmitInfo D3D12FenceSubmitInfoKHR = ()
  Extends SubmitInfo DeviceGroupSubmitInfo = ()
  Extends SubmitInfo ProtectedSubmitInfo = ()
  Extends SubmitInfo TimelineSemaphoreSubmitInfo = ()
  Extends SubmitInfo PerformanceQuerySubmitInfoKHR = ()
  Extends SubmitInfo AmigoProfilingSubmitInfoSEC = ()
  Extends SubmitInfo FrameBoundaryEXT = ()
  Extends SubmitInfo LatencySubmissionPresentIdNV = ()
  Extends SubmitInfo2 Win32KeyedMutexAcquireReleaseInfoNV = ()
  Extends SubmitInfo2 Win32KeyedMutexAcquireReleaseInfoKHR = ()
  Extends SubmitInfo2 PerformanceQuerySubmitInfoKHR = ()
  Extends SubmitInfo2 FrameBoundaryEXT = ()
  Extends SubmitInfo2 LatencySubmissionPresentIdNV = ()
  Extends SubpassDependency2 MemoryBarrier2 = ()
  Extends SubpassDescription2 SubpassDescriptionDepthStencilResolve = ()
  Extends SubpassDescription2 FragmentShadingRateAttachmentInfoKHR = ()
  Extends SubpassDescription2 MultisampledRenderToSingleSampledInfoEXT = ()
  Extends SubpassDescription2 RenderPassCreationControlEXT = ()
  Extends SubpassDescription2 RenderPassSubpassFeedbackCreateInfoEXT = ()
  Extends SubpassEndInfo SubpassFragmentDensityMapOffsetEndInfoQCOM = ()
  Extends SubresourceLayout2KHR SubresourceHostMemcpySizeEXT = ()
  Extends SubresourceLayout2KHR ImageCompressionPropertiesEXT = ()
  Extends SurfaceCapabilities2KHR DisplayNativeHdrSurfaceCapabilitiesAMD = ()
  Extends SurfaceCapabilities2KHR SharedPresentSurfaceCapabilitiesKHR = ()
  Extends SurfaceCapabilities2KHR SurfaceProtectedCapabilitiesKHR = ()
  Extends SurfaceCapabilities2KHR SurfaceCapabilitiesFullScreenExclusiveEXT = ()
  Extends SurfaceCapabilities2KHR SurfaceCapabilitiesPresentBarrierNV = ()
  Extends SurfaceCapabilities2KHR SurfacePresentScalingCapabilitiesEXT = ()
  Extends SurfaceCapabilities2KHR SurfacePresentModeCompatibilityEXT = ()
  Extends SurfaceCapabilities2KHR LatencySurfaceCapabilitiesNV = ()
  Extends SurfaceFormat2KHR ImageCompressionPropertiesEXT = ()
  Extends SwapchainCreateInfoKHR SwapchainCounterCreateInfoEXT = ()
  Extends SwapchainCreateInfoKHR DeviceGroupSwapchainCreateInfoKHR = ()
  Extends SwapchainCreateInfoKHR SwapchainDisplayNativeHdrCreateInfoAMD = ()
  Extends SwapchainCreateInfoKHR ImageFormatListCreateInfo = ()
  Extends SwapchainCreateInfoKHR SurfaceFullScreenExclusiveInfoEXT = ()
  Extends SwapchainCreateInfoKHR SurfaceFullScreenExclusiveWin32InfoEXT = ()
  Extends SwapchainCreateInfoKHR SwapchainPresentBarrierCreateInfoNV = ()
  Extends SwapchainCreateInfoKHR ImageCompressionControlEXT = ()
  Extends SwapchainCreateInfoKHR SwapchainPresentModesCreateInfoEXT = ()
  Extends SwapchainCreateInfoKHR SwapchainPresentScalingCreateInfoEXT = ()
  Extends SwapchainCreateInfoKHR SwapchainLatencyCreateInfoNV = ()
  Extends WriteDescriptorSet WriteDescriptorSetInlineUniformBlock = ()
  Extends WriteDescriptorSet WriteDescriptorSetAccelerationStructureKHR = ()
  Extends WriteDescriptorSet WriteDescriptorSetAccelerationStructureNV = ()
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
      $ peekChainHead @a (case baseOut of BaseOutStructure{sType} -> sType)
                         (castPtr @BaseOutStructure @() p)
      $ \head' -> peekSomeChain @a (case baseOut of BaseOutStructure{next} -> next)
                                  (\tail' -> c (head', tail'))

peekChainHead
  :: forall a b
   . Extensible a
  => StructureType
  -> Ptr ()
  -> (forall e . (Extends a e, ToCStruct e, Show e) => e -> b)
  -> IO b
peekChainHead ty p c = case ty of
  STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO_KHR -> go @BufferUsageFlags2CreateInfoKHR
  STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO -> go @(ShaderModuleCreateInfo '[])
  STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO_KHR -> go @PipelineCreateFlags2CreateInfoKHR
  STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR -> go @DisplayPresentInfoKHR
  STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT -> go @DebugReportCallbackCreateInfoEXT
  STRUCTURE_TYPE_VALIDATION_FLAGS_EXT -> go @ValidationFlagsEXT
  STRUCTURE_TYPE_VALIDATION_FEATURES_EXT -> go @ValidationFeaturesEXT
  STRUCTURE_TYPE_LAYER_SETTINGS_CREATE_INFO_EXT -> go @LayerSettingsCreateInfoEXT
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
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV -> go @PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV
  STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO -> go @DevicePrivateDataCreateInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES -> go @PhysicalDevicePrivateDataFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV -> go @PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_PROPERTIES_EXT -> go @PhysicalDeviceMultiDrawPropertiesEXT
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
  STRUCTURE_TYPE_IMPORT_MEMORY_ZIRCON_HANDLE_INFO_FUCHSIA -> go @ImportMemoryZirconHandleInfoFUCHSIA
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
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR -> go @PhysicalDevicePresentIdFeaturesKHR
  STRUCTURE_TYPE_PRESENT_ID_KHR -> go @PresentIdKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_FEATURES_KHR -> go @PhysicalDevicePresentWaitFeaturesKHR
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
  STRUCTURE_TYPE_IMAGE_VIEW_SLICED_CREATE_INFO_EXT -> go @ImageViewSlicedCreateInfoEXT
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
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_FEATURES_EXT -> go @PhysicalDeviceMultiDrawFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT -> go @PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT -> go @PipelineColorBlendAdvancedStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES -> go @PhysicalDeviceInlineUniformBlockFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES -> go @PhysicalDeviceInlineUniformBlockProperties
  STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK -> go @WriteDescriptorSetInlineUniformBlock
  STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO -> go @DescriptorPoolInlineUniformBlockCreateInfo
  STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV -> go @PipelineCoverageModulationStateCreateInfoNV
  STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO -> go @ImageFormatListCreateInfo
  STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT -> go @ShaderModuleValidationCacheCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES -> go @PhysicalDeviceMaintenance3Properties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES -> go @PhysicalDeviceMaintenance4Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES -> go @PhysicalDeviceMaintenance4Properties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES_KHR -> go @PhysicalDeviceMaintenance5FeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES_KHR -> go @PhysicalDeviceMaintenance5PropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES -> go @PhysicalDeviceShaderDrawParametersFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES -> go @PhysicalDeviceShaderFloat16Int8Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES -> go @PhysicalDeviceFloatControlsProperties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES -> go @PhysicalDeviceHostQueryResetFeatures
  STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR -> go @DeviceQueueGlobalPriorityCreateInfoKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR -> go @PhysicalDeviceGlobalPriorityQueryFeaturesKHR
  STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR -> go @QueueFamilyGlobalPriorityPropertiesKHR
  STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT -> go @DebugUtilsObjectNameInfoEXT
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
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT -> go @PhysicalDeviceShaderAtomicFloat2FeaturesEXT
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
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV -> go @PhysicalDeviceShaderImageFootprintFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV -> go @PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV -> go @PhysicalDeviceCopyMemoryIndirectFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV -> go @PhysicalDeviceCopyMemoryIndirectPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV -> go @PhysicalDeviceMemoryDecompressionFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV -> go @PhysicalDeviceMemoryDecompressionPropertiesNV
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV -> go @PipelineViewportShadingRateImageStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV -> go @PhysicalDeviceShadingRateImageFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV -> go @PhysicalDeviceShadingRateImagePropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_INVOCATION_MASK_FEATURES_HUAWEI -> go @PhysicalDeviceInvocationMaskFeaturesHUAWEI
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV -> go @PipelineViewportCoarseSampleOrderStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV -> go @PhysicalDeviceMeshShaderFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV -> go @PhysicalDeviceMeshShaderPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_EXT -> go @PhysicalDeviceMeshShaderFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_EXT -> go @PhysicalDeviceMeshShaderPropertiesEXT
  STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR -> go @WriteDescriptorSetAccelerationStructureKHR
  STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV -> go @WriteDescriptorSetAccelerationStructureNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_FEATURES_KHR -> go @PhysicalDeviceAccelerationStructureFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR -> go @PhysicalDeviceRayTracingPipelineFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_QUERY_FEATURES_KHR -> go @PhysicalDeviceRayQueryFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ACCELERATION_STRUCTURE_PROPERTIES_KHR -> go @PhysicalDeviceAccelerationStructurePropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR -> go @PhysicalDeviceRayTracingPipelinePropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV -> go @PhysicalDeviceRayTracingPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR -> go @PhysicalDeviceRayTracingMaintenance1FeaturesKHR
  STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT -> go @DrmFormatModifierPropertiesListEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT -> go @PhysicalDeviceImageDrmFormatModifierInfoEXT
  STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT -> go @ImageDrmFormatModifierListCreateInfoEXT
  STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT -> go @ImageDrmFormatModifierExplicitCreateInfoEXT
  STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO -> go @ImageStencilUsageCreateInfo
  STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD -> go @DeviceMemoryOverallocationCreateInfoAMD
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT -> go @PhysicalDeviceFragmentDensityMapFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT -> go @PhysicalDeviceFragmentDensityMap2FeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM -> go @PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT -> go @PhysicalDeviceFragmentDensityMapPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT -> go @PhysicalDeviceFragmentDensityMap2PropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM -> go @PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM
  STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT -> go @RenderPassFragmentDensityMapCreateInfoEXT
  STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM -> go @SubpassFragmentDensityMapOffsetEndInfoQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES -> go @PhysicalDeviceScalarBlockLayoutFeatures
  STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR -> go @SurfaceProtectedCapabilitiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES -> go @PhysicalDeviceUniformBufferStandardLayoutFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT -> go @PhysicalDeviceDepthClipEnableFeaturesEXT
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT -> go @PipelineRasterizationDepthClipStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT -> go @PhysicalDeviceMemoryBudgetPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT -> go @PhysicalDeviceMemoryPriorityFeaturesEXT
  STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT -> go @MemoryPriorityAllocateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PAGEABLE_DEVICE_LOCAL_MEMORY_FEATURES_EXT -> go @PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES -> go @PhysicalDeviceBufferDeviceAddressFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT -> go @PhysicalDeviceBufferDeviceAddressFeaturesEXT
  STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO -> go @BufferOpaqueCaptureAddressCreateInfo
  STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT -> go @BufferDeviceAddressCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT -> go @PhysicalDeviceImageViewImageFormatInfoEXT
  STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT -> go @FilterCubicImageViewImageFormatPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES -> go @PhysicalDeviceImagelessFramebufferFeatures
  STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO -> go @FramebufferAttachmentsCreateInfo
  STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO -> go @RenderPassAttachmentBeginInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES -> go @PhysicalDeviceTextureCompressionASTCHDRFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV -> go @PhysicalDeviceCooperativeMatrixFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV -> go @PhysicalDeviceCooperativeMatrixPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT -> go @PhysicalDeviceYcbcrImageArraysFeaturesEXT
  STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP -> go @PresentFrameTokenGGP
  STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO -> go @PipelineCreationFeedbackCreateInfo
  STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT -> go @SurfaceFullScreenExclusiveInfoEXT
  STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT -> go @SurfaceFullScreenExclusiveWin32InfoEXT
  STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT -> go @SurfaceCapabilitiesFullScreenExclusiveEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV -> go @PhysicalDevicePresentBarrierFeaturesNV
  STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV -> go @SurfaceCapabilitiesPresentBarrierNV
  STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV -> go @SwapchainPresentBarrierCreateInfoNV
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
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT -> go @PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT
  STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT -> go @AttachmentDescriptionStencilLayout
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR -> go @PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES -> go @PhysicalDeviceShaderDemoteToHelperInvocationFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT -> go @PhysicalDeviceTexelBufferAlignmentFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES -> go @PhysicalDeviceTexelBufferAlignmentProperties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES -> go @PhysicalDeviceSubgroupSizeControlFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES -> go @PhysicalDeviceSubgroupSizeControlProperties
  STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO -> go @PipelineShaderStageRequiredSubgroupSizeCreateInfo
  STRUCTURE_TYPE_SUBPASS_SHADING_PIPELINE_CREATE_INFO_HUAWEI -> go @SubpassShadingPipelineCreateInfoHUAWEI
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_PROPERTIES_HUAWEI -> go @PhysicalDeviceSubpassShadingPropertiesHUAWEI
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_PROPERTIES_HUAWEI -> go @PhysicalDeviceClusterCullingShaderPropertiesHUAWEI
  STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO -> go @MemoryOpaqueCaptureAddressAllocateInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT -> go @PhysicalDeviceLineRasterizationFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT -> go @PhysicalDeviceLineRasterizationPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT -> go @PipelineRasterizationLineStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES -> go @PhysicalDevicePipelineCreationCacheControlFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES -> go @PhysicalDeviceVulkan11Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES -> go @PhysicalDeviceVulkan11Properties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES -> go @PhysicalDeviceVulkan12Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES -> go @PhysicalDeviceVulkan12Properties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_FEATURES -> go @PhysicalDeviceVulkan13Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_3_PROPERTIES -> go @PhysicalDeviceVulkan13Properties
  STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD -> go @PipelineCompilerControlCreateInfoAMD
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD -> go @PhysicalDeviceCoherentMemoryFeaturesAMD
  STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT -> throwIO $ IOError Nothing InvalidArgument "peekChainHead" ("struct type STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT contains an undiscriminated union (ClearColorValue) and can't be safely peeked") Nothing Nothing
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT -> go @PhysicalDeviceCustomBorderColorPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT -> go @PhysicalDeviceCustomBorderColorFeaturesEXT
  STRUCTURE_TYPE_SAMPLER_BORDER_COLOR_COMPONENT_MAPPING_CREATE_INFO_EXT -> go @SamplerBorderColorComponentMappingCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BORDER_COLOR_SWIZZLE_FEATURES_EXT -> go @PhysicalDeviceBorderColorSwizzleFeaturesEXT
  STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR -> go @PipelineLibraryCreateInfoKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT -> go @PhysicalDeviceExtendedDynamicStateFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT -> go @PhysicalDeviceExtendedDynamicState2FeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT -> go @PhysicalDeviceExtendedDynamicState3FeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT -> go @PhysicalDeviceExtendedDynamicState3PropertiesEXT
  STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM -> go @RenderPassTransformBeginInfoQCOM
  STRUCTURE_TYPE_COPY_COMMAND_TRANSFORM_INFO_QCOM -> go @CopyCommandTransformInfoQCOM
  STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM -> go @CommandBufferInheritanceRenderPassTransformInfoQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV -> go @PhysicalDeviceDiagnosticsConfigFeaturesNV
  STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV -> go @DeviceDiagnosticsConfigCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES -> go @PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR -> go @PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT -> go @PhysicalDeviceRobustness2FeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT -> go @PhysicalDeviceRobustness2PropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES -> go @PhysicalDeviceImageRobustnessFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_WORKGROUP_MEMORY_EXPLICIT_LAYOUT_FEATURES_KHR -> go @PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR -> go @PhysicalDevicePortabilitySubsetFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR -> go @PhysicalDevicePortabilitySubsetPropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT -> go @PhysicalDevice4444FormatsFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_FEATURES_HUAWEI -> go @PhysicalDeviceSubpassShadingFeaturesHUAWEI
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_FEATURES_HUAWEI -> go @(PhysicalDeviceClusterCullingShaderFeaturesHUAWEI '[])
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CLUSTER_CULLING_SHADER_VRS_FEATURES_HUAWEI -> go @PhysicalDeviceClusterCullingShaderVrsFeaturesHUAWEI
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT -> go @PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
  STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR -> go @FragmentShadingRateAttachmentInfoKHR
  STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR -> go @PipelineFragmentShadingRateStateCreateInfoKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR -> go @PhysicalDeviceFragmentShadingRateFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR -> go @PhysicalDeviceFragmentShadingRatePropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TERMINATE_INVOCATION_FEATURES -> go @PhysicalDeviceShaderTerminateInvocationFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV -> go @PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV -> go @PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
  STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV -> go @PipelineFragmentShadingRateEnumStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_2D_VIEW_OF_3D_FEATURES_EXT -> go @PhysicalDeviceImage2DViewOf3DFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_SLICED_VIEW_OF_3D_FEATURES_EXT -> go @PhysicalDeviceImageSlicedViewOf3DFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT -> go @PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MUTABLE_DESCRIPTOR_TYPE_FEATURES_EXT -> go @PhysicalDeviceMutableDescriptorTypeFeaturesEXT
  STRUCTURE_TYPE_MUTABLE_DESCRIPTOR_TYPE_CREATE_INFO_EXT -> go @MutableDescriptorTypeCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_CONTROL_FEATURES_EXT -> go @PhysicalDeviceDepthClipControlFeaturesEXT
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_DEPTH_CLIP_CONTROL_CREATE_INFO_EXT -> go @PipelineViewportDepthClipControlCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT -> go @PhysicalDeviceVertexInputDynamicStateFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV -> go @PhysicalDeviceExternalMemoryRDMAFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COLOR_WRITE_ENABLE_FEATURES_EXT -> go @PhysicalDeviceColorWriteEnableFeaturesEXT
  STRUCTURE_TYPE_PIPELINE_COLOR_WRITE_CREATE_INFO_EXT -> go @PipelineColorWriteCreateInfoEXT
  STRUCTURE_TYPE_MEMORY_BARRIER_2 -> go @MemoryBarrier2
  STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_2_NV -> go @QueueFamilyCheckpointProperties2NV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES -> go @PhysicalDeviceSynchronization2Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT -> go @PhysicalDeviceHostImageCopyFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT -> go @PhysicalDeviceHostImageCopyPropertiesEXT
  STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT -> go @SubresourceHostMemcpySizeEXT
  STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT -> go @HostImageCopyDevicePerformanceQueryEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVES_GENERATED_QUERY_FEATURES_EXT -> go @PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_DITHERING_FEATURES_EXT -> go @PhysicalDeviceLegacyDitheringFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_FEATURES_EXT -> go @PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT
  STRUCTURE_TYPE_SUBPASS_RESOLVE_PERFORMANCE_QUERY_EXT -> go @SubpassResolvePerformanceQueryEXT
  STRUCTURE_TYPE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_INFO_EXT -> go @MultisampledRenderToSingleSampledInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROTECTED_ACCESS_FEATURES_EXT -> go @PhysicalDevicePipelineProtectedAccessFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_INHERITED_VIEWPORT_SCISSOR_FEATURES_NV -> go @PhysicalDeviceInheritedViewportScissorFeaturesNV
  STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_VIEWPORT_SCISSOR_INFO_NV -> go @CommandBufferInheritanceViewportScissorInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT -> go @PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT -> go @PhysicalDeviceProvokingVertexFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT -> go @PhysicalDeviceProvokingVertexPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT -> go @PipelineRasterizationProvokingVertexStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_FEATURES_EXT -> go @PhysicalDeviceDescriptorBufferFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_PROPERTIES_EXT -> go @PhysicalDeviceDescriptorBufferPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_BUFFER_DENSITY_MAP_PROPERTIES_EXT -> go @PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT
  STRUCTURE_TYPE_DESCRIPTOR_BUFFER_BINDING_PUSH_DESCRIPTOR_BUFFER_HANDLE_EXT -> go @DescriptorBufferBindingPushDescriptorBufferHandleEXT
  STRUCTURE_TYPE_OPAQUE_CAPTURE_DESCRIPTOR_DATA_CREATE_INFO_EXT -> go @OpaqueCaptureDescriptorDataCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_FEATURES -> go @PhysicalDeviceShaderIntegerDotProductFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_DOT_PRODUCT_PROPERTIES -> go @PhysicalDeviceShaderIntegerDotProductProperties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT -> go @PhysicalDeviceDrmPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR -> go @PhysicalDeviceFragmentShaderBarycentricFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR -> go @PhysicalDeviceFragmentShaderBarycentricPropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MOTION_BLUR_FEATURES_NV -> go @PhysicalDeviceRayTracingMotionBlurFeaturesNV
  STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV -> throwIO $ IOError Nothing InvalidArgument "peekChainHead" ("struct type STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_MOTION_TRIANGLES_DATA_NV contains an undiscriminated union (DeviceOrHostAddressConstKHR) and can't be safely peeked") Nothing Nothing
  STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MOTION_INFO_NV -> go @AccelerationStructureMotionInfoNV
  STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA -> go @ImportMemoryBufferCollectionFUCHSIA
  STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA -> go @BufferCollectionImageCreateInfoFUCHSIA
  STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA -> go @BufferCollectionBufferCreateInfoFUCHSIA
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT -> go @PhysicalDeviceRGBA10X6FormatsFeaturesEXT
  STRUCTURE_TYPE_FORMAT_PROPERTIES_3 -> go @FormatProperties3
  STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_2_EXT -> go @DrmFormatModifierPropertiesList2EXT
  STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_2_ANDROID -> go @AndroidHardwareBufferFormatProperties2ANDROID
  STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO -> go @PipelineRenderingCreateInfo
  STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR -> go @RenderingFragmentShadingRateAttachmentInfoKHR
  STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT -> go @RenderingFragmentDensityMapAttachmentInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES -> go @PhysicalDeviceDynamicRenderingFeatures
  STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO -> go @CommandBufferInheritanceRenderingInfo
  STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD -> go @AttachmentSampleCountInfoAMD
  STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX -> go @MultiviewPerViewAttributesInfoNVX
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_MIN_LOD_FEATURES_EXT -> go @PhysicalDeviceImageViewMinLodFeaturesEXT
  STRUCTURE_TYPE_IMAGE_VIEW_MIN_LOD_CREATE_INFO_EXT -> go @ImageViewMinLodCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT -> go @PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_LINEAR_COLOR_ATTACHMENT_FEATURES_NV -> go @PhysicalDeviceLinearColorAttachmentFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT -> go @PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT -> go @PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT
  STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT -> go @GraphicsPipelineLibraryCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_SET_HOST_MAPPING_FEATURES_VALVE -> go @PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE
  STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT -> go @PhysicalDeviceNestedCommandBufferFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT -> go @PhysicalDeviceNestedCommandBufferPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT -> go @PhysicalDeviceShaderModuleIdentifierFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT -> go @PhysicalDeviceShaderModuleIdentifierPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT -> go @PipelineShaderStageModuleIdentifierCreateInfoEXT
  STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT -> go @ImageCompressionControlEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT -> go @PhysicalDeviceImageCompressionControlFeaturesEXT
  STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT -> go @ImageCompressionPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT -> go @PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT
  STRUCTURE_TYPE_RENDER_PASS_CREATION_CONTROL_EXT -> go @RenderPassCreationControlEXT
  STRUCTURE_TYPE_RENDER_PASS_CREATION_FEEDBACK_CREATE_INFO_EXT -> go @RenderPassCreationFeedbackCreateInfoEXT
  STRUCTURE_TYPE_RENDER_PASS_SUBPASS_FEEDBACK_CREATE_INFO_EXT -> go @RenderPassSubpassFeedbackCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_MERGE_FEEDBACK_FEATURES_EXT -> go @PhysicalDeviceSubpassMergeFeedbackFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_FEATURES_EXT -> go @PhysicalDeviceOpacityMicromapFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_OPACITY_MICROMAP_PROPERTIES_EXT -> go @PhysicalDeviceOpacityMicromapPropertiesEXT
  STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT -> throwIO $ IOError Nothing InvalidArgument "peekChainHead" ("struct type STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_OPACITY_MICROMAP_EXT contains an undiscriminated union (DeviceOrHostAddressConstKHR) and can't be safely peeked") Nothing Nothing
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_FEATURES_NV -> go @PhysicalDeviceDisplacementMicromapFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DISPLACEMENT_MICROMAP_PROPERTIES_NV -> go @PhysicalDeviceDisplacementMicromapPropertiesNV
  STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV -> throwIO $ IOError Nothing InvalidArgument "peekChainHead" ("struct type STRUCTURE_TYPE_ACCELERATION_STRUCTURE_TRIANGLES_DISPLACEMENT_MICROMAP_NV contains an undiscriminated union (DeviceOrHostAddressConstKHR) and can't be safely peeked") Nothing Nothing
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT -> go @PhysicalDevicePipelinePropertiesFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_EARLY_AND_LATE_FRAGMENT_TESTS_FEATURES_AMD -> go @PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD
  STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT -> go @ExternalMemoryAcquireUnmodifiedEXT
  STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT -> go @ExportMetalObjectCreateInfoEXT
  STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT -> go @ExportMetalDeviceInfoEXT
  STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT -> go @ExportMetalCommandQueueInfoEXT
  STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT -> go @ExportMetalBufferInfoEXT
  STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT -> go @ImportMetalBufferInfoEXT
  STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT -> go @ExportMetalTextureInfoEXT
  STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT -> go @ImportMetalTextureInfoEXT
  STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT -> go @ExportMetalIOSurfaceInfoEXT
  STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT -> go @ImportMetalIOSurfaceInfoEXT
  STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT -> go @ExportMetalSharedEventInfoEXT
  STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT -> go @ImportMetalSharedEventInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT -> go @PhysicalDeviceNonSeamlessCubeMapFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT -> go @PhysicalDevicePipelineRobustnessFeaturesEXT
  STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT -> go @PipelineRobustnessCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT -> go @PhysicalDevicePipelineRobustnessPropertiesEXT
  STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM -> go @ImageViewSampleWeightCreateInfoQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM -> go @PhysicalDeviceImageProcessingFeaturesQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM -> go @PhysicalDeviceImageProcessingPropertiesQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM -> go @PhysicalDeviceTilePropertiesFeaturesQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_AMIGO_PROFILING_FEATURES_SEC -> go @PhysicalDeviceAmigoProfilingFeaturesSEC
  STRUCTURE_TYPE_AMIGO_PROFILING_SUBMIT_INFO_SEC -> go @AmigoProfilingSubmitInfoSEC
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT -> go @PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT -> go @PhysicalDeviceDepthClampZeroOneFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ADDRESS_BINDING_REPORT_FEATURES_EXT -> go @PhysicalDeviceAddressBindingReportFeaturesEXT
  STRUCTURE_TYPE_DEVICE_ADDRESS_BINDING_CALLBACK_DATA_EXT -> go @DeviceAddressBindingCallbackDataEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV -> go @PhysicalDeviceOpticalFlowFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV -> go @PhysicalDeviceOpticalFlowPropertiesNV
  STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV -> go @OpticalFlowImageFormatInfoNV
  STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV -> go @OpticalFlowSessionCreatePrivateDataInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT -> go @PhysicalDeviceFaultFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_LIBRARY_GROUP_HANDLES_FEATURES_EXT -> go @PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT
  STRUCTURE_TYPE_DEPTH_BIAS_REPRESENTATION_INFO_EXT -> go @DepthBiasRepresentationInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_PROPERTIES_ARM -> go @PhysicalDeviceShaderCoreBuiltinsPropertiesARM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_BUILTINS_FEATURES_ARM -> go @PhysicalDeviceShaderCoreBuiltinsFeaturesARM
  STRUCTURE_TYPE_FRAME_BOUNDARY_EXT -> go @FrameBoundaryEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAME_BOUNDARY_FEATURES_EXT -> go @PhysicalDeviceFrameBoundaryFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT -> go @PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT
  STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT -> go @SurfacePresentModeEXT
  STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT -> go @SurfacePresentScalingCapabilitiesEXT
  STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT -> go @SurfacePresentModeCompatibilityEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT -> go @PhysicalDeviceSwapchainMaintenance1FeaturesEXT
  STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT -> go @SwapchainPresentFenceInfoEXT
  STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT -> go @SwapchainPresentModesCreateInfoEXT
  STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT -> go @SwapchainPresentModeInfoEXT
  STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT -> go @SwapchainPresentScalingCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_BIAS_CONTROL_FEATURES_EXT -> go @PhysicalDeviceDepthBiasControlFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_FEATURES_NV -> go @PhysicalDeviceRayTracingInvocationReorderFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_INVOCATION_REORDER_PROPERTIES_NV -> go @PhysicalDeviceRayTracingInvocationReorderPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_FEATURES_NV -> go @PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_SPARSE_ADDRESS_SPACE_PROPERTIES_NV -> go @PhysicalDeviceExtendedSparseAddressSpacePropertiesNV
  STRUCTURE_TYPE_DIRECT_DRIVER_LOADING_LIST_LUNARG -> go @DirectDriverLoadingListLUNARG
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM -> go @PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_POSITION_FETCH_FEATURES_KHR -> go @PhysicalDeviceRayTracingPositionFetchFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_ARM -> go @PhysicalDeviceShaderCorePropertiesARM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM -> go @PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM
  STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM -> go @MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM
  STRUCTURE_TYPE_QUERY_LOW_LATENCY_SUPPORT_NV -> go @QueryLowLatencySupportNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_FEATURES_EXT -> go @PhysicalDeviceShaderObjectFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_OBJECT_PROPERTIES_EXT -> go @PhysicalDeviceShaderObjectPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_FEATURES_EXT -> go @PhysicalDeviceShaderTileImageFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_TILE_IMAGE_PROPERTIES_EXT -> go @PhysicalDeviceShaderTileImagePropertiesEXT
  STRUCTURE_TYPE_IMPORT_SCREEN_BUFFER_INFO_QNX -> go @ImportScreenBufferInfoQNX
  STRUCTURE_TYPE_SCREEN_BUFFER_FORMAT_PROPERTIES_QNX -> go @ScreenBufferFormatPropertiesQNX
  STRUCTURE_TYPE_EXTERNAL_FORMAT_QNX -> go @ExternalFormatQNX
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCREEN_BUFFER_FEATURES_QNX -> go @PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR -> go @PhysicalDeviceCooperativeMatrixFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR -> go @PhysicalDeviceCooperativeMatrixPropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_PROPERTIES_AMDX -> go @PhysicalDeviceShaderEnqueuePropertiesAMDX
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ENQUEUE_FEATURES_AMDX -> go @PhysicalDeviceShaderEnqueueFeaturesAMDX
  STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_NODE_CREATE_INFO_AMDX -> go @PipelineShaderStageNodeCreateInfoAMDX
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM -> go @PhysicalDeviceCubicClampFeaturesQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_DEGAMMA_FEATURES_QCOM -> go @PhysicalDeviceYcbcrDegammaFeaturesQCOM
  STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_YCBCR_DEGAMMA_CREATE_INFO_QCOM -> go @SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM -> go @PhysicalDeviceCubicWeightsFeaturesQCOM
  STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM -> go @SamplerCubicWeightsCreateInfoQCOM
  STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM -> go @BlitImageCubicWeightsInfoQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM -> go @PhysicalDeviceImageProcessing2FeaturesQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM -> go @PhysicalDeviceImageProcessing2PropertiesQCOM
  STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM -> go @SamplerBlockMatchWindowCreateInfoQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_POOL_OVERALLOCATION_FEATURES_NV -> go @PhysicalDeviceDescriptorPoolOverallocationFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT -> go @PhysicalDeviceLayeredDriverPropertiesMSFT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_FEATURES_ANDROID -> go @PhysicalDeviceExternalFormatResolveFeaturesANDROID
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FORMAT_RESOLVE_PROPERTIES_ANDROID -> go @PhysicalDeviceExternalFormatResolvePropertiesANDROID
  STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_RESOLVE_PROPERTIES_ANDROID -> go @AndroidHardwareBufferFormatResolvePropertiesANDROID
  STRUCTURE_TYPE_LATENCY_SUBMISSION_PRESENT_ID_NV -> go @LatencySubmissionPresentIdNV
  STRUCTURE_TYPE_SWAPCHAIN_LATENCY_CREATE_INFO_NV -> go @SwapchainLatencyCreateInfoNV
  STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV -> go @LatencySurfaceCapabilitiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV -> go @PhysicalDeviceCudaKernelLaunchFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV -> go @PhysicalDeviceCudaKernelLaunchPropertiesNV
  STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM -> go @DeviceQueueShaderCoreControlCreateInfoARM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM -> go @PhysicalDeviceSchedulingControlsFeaturesARM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM -> go @PhysicalDeviceSchedulingControlsPropertiesARM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RELAXED_LINE_RASTERIZATION_FEATURES_IMG -> go @PhysicalDeviceRelaxedLineRasterizationFeaturesIMG
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_FEATURES_ARM -> go @PhysicalDeviceRenderPassStripedFeaturesARM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RENDER_PASS_STRIPED_PROPERTIES_ARM -> go @PhysicalDeviceRenderPassStripedPropertiesARM
  STRUCTURE_TYPE_RENDER_PASS_STRIPE_BEGIN_INFO_ARM -> go @RenderPassStripeBeginInfoARM
  STRUCTURE_TYPE_RENDER_PASS_STRIPE_SUBMIT_INFO_ARM -> go @RenderPassStripeSubmitInfoARM
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
            <> extensibleTypeName @a
            <> " with "
            <> show ty
            )
            Nothing
            Nothing
          )
          r

class Extensible (a :: [Type] -> Type) where
  extensibleTypeName :: String
  -- ^ For error reporting an invalid extension
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
{-# complete (::&) :: BufferUsageFlags2CreateInfoKHR #-}
{-# complete (::&) :: ShaderModuleCreateInfo #-}
{-# complete (::&) :: PipelineCreateFlags2CreateInfoKHR #-}
{-# complete (::&) :: DisplayPresentInfoKHR #-}
{-# complete (::&) :: DebugReportCallbackCreateInfoEXT #-}
{-# complete (::&) :: ValidationFlagsEXT #-}
{-# complete (::&) :: ValidationFeaturesEXT #-}
{-# complete (::&) :: LayerSettingsCreateInfoEXT #-}
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
{-# complete (::&) :: PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV #-}
{-# complete (::&) :: DevicePrivateDataCreateInfo #-}
{-# complete (::&) :: PhysicalDevicePrivateDataFeatures #-}
{-# complete (::&) :: PhysicalDeviceDeviceGeneratedCommandsPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceMultiDrawPropertiesEXT #-}
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
{-# complete (::&) :: ImportMemoryZirconHandleInfoFUCHSIA #-}
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
{-# complete (::&) :: PhysicalDevicePresentIdFeaturesKHR #-}
{-# complete (::&) :: PresentIdKHR #-}
{-# complete (::&) :: PhysicalDevicePresentWaitFeaturesKHR #-}
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
{-# complete (::&) :: ImageViewSlicedCreateInfoEXT #-}
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
{-# complete (::&) :: PhysicalDeviceMultiDrawFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT #-}
{-# complete (::&) :: PipelineColorBlendAdvancedStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceInlineUniformBlockFeatures #-}
{-# complete (::&) :: PhysicalDeviceInlineUniformBlockProperties #-}
{-# complete (::&) :: WriteDescriptorSetInlineUniformBlock #-}
{-# complete (::&) :: DescriptorPoolInlineUniformBlockCreateInfo #-}
{-# complete (::&) :: PipelineCoverageModulationStateCreateInfoNV #-}
{-# complete (::&) :: ImageFormatListCreateInfo #-}
{-# complete (::&) :: ShaderModuleValidationCacheCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceMaintenance3Properties #-}
{-# complete (::&) :: PhysicalDeviceMaintenance4Features #-}
{-# complete (::&) :: PhysicalDeviceMaintenance4Properties #-}
{-# complete (::&) :: PhysicalDeviceMaintenance5FeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceMaintenance5PropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceShaderDrawParametersFeatures #-}
{-# complete (::&) :: PhysicalDeviceShaderFloat16Int8Features #-}
{-# complete (::&) :: PhysicalDeviceFloatControlsProperties #-}
{-# complete (::&) :: PhysicalDeviceHostQueryResetFeatures #-}
{-# complete (::&) :: DeviceQueueGlobalPriorityCreateInfoKHR #-}
{-# complete (::&) :: PhysicalDeviceGlobalPriorityQueryFeaturesKHR #-}
{-# complete (::&) :: QueueFamilyGlobalPriorityPropertiesKHR #-}
{-# complete (::&) :: DebugUtilsObjectNameInfoEXT #-}
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
{-# complete (::&) :: PhysicalDeviceShaderAtomicFloat2FeaturesEXT #-}
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
{-# complete (::&) :: PhysicalDeviceShaderImageFootprintFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceCopyMemoryIndirectFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceCopyMemoryIndirectPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceMemoryDecompressionFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceMemoryDecompressionPropertiesNV #-}
{-# complete (::&) :: PipelineViewportShadingRateImageStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceShadingRateImageFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceShadingRateImagePropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceInvocationMaskFeaturesHUAWEI #-}
{-# complete (::&) :: PipelineViewportCoarseSampleOrderStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceMeshShaderFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceMeshShaderPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceMeshShaderFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceMeshShaderPropertiesEXT #-}
{-# complete (::&) :: WriteDescriptorSetAccelerationStructureKHR #-}
{-# complete (::&) :: WriteDescriptorSetAccelerationStructureNV #-}
{-# complete (::&) :: PhysicalDeviceAccelerationStructureFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceRayTracingPipelineFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceRayQueryFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceAccelerationStructurePropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceRayTracingPipelinePropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceRayTracingPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceRayTracingMaintenance1FeaturesKHR #-}
{-# complete (::&) :: DrmFormatModifierPropertiesListEXT #-}
{-# complete (::&) :: PhysicalDeviceImageDrmFormatModifierInfoEXT #-}
{-# complete (::&) :: ImageDrmFormatModifierListCreateInfoEXT #-}
{-# complete (::&) :: ImageDrmFormatModifierExplicitCreateInfoEXT #-}
{-# complete (::&) :: ImageStencilUsageCreateInfo #-}
{-# complete (::&) :: DeviceMemoryOverallocationCreateInfoAMD #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMapFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMap2FeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMapPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMap2PropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM #-}
{-# complete (::&) :: RenderPassFragmentDensityMapCreateInfoEXT #-}
{-# complete (::&) :: SubpassFragmentDensityMapOffsetEndInfoQCOM #-}
{-# complete (::&) :: PhysicalDeviceScalarBlockLayoutFeatures #-}
{-# complete (::&) :: SurfaceProtectedCapabilitiesKHR #-}
{-# complete (::&) :: PhysicalDeviceUniformBufferStandardLayoutFeatures #-}
{-# complete (::&) :: PhysicalDeviceDepthClipEnableFeaturesEXT #-}
{-# complete (::&) :: PipelineRasterizationDepthClipStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceMemoryBudgetPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceMemoryPriorityFeaturesEXT #-}
{-# complete (::&) :: MemoryPriorityAllocateInfoEXT #-}
{-# complete (::&) :: PhysicalDevicePageableDeviceLocalMemoryFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceBufferDeviceAddressFeatures #-}
{-# complete (::&) :: PhysicalDeviceBufferDeviceAddressFeaturesEXT #-}
{-# complete (::&) :: BufferOpaqueCaptureAddressCreateInfo #-}
{-# complete (::&) :: BufferDeviceAddressCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceImageViewImageFormatInfoEXT #-}
{-# complete (::&) :: FilterCubicImageViewImageFormatPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceImagelessFramebufferFeatures #-}
{-# complete (::&) :: FramebufferAttachmentsCreateInfo #-}
{-# complete (::&) :: RenderPassAttachmentBeginInfo #-}
{-# complete (::&) :: PhysicalDeviceTextureCompressionASTCHDRFeatures #-}
{-# complete (::&) :: PhysicalDeviceCooperativeMatrixFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceCooperativeMatrixPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceYcbcrImageArraysFeaturesEXT #-}
{-# complete (::&) :: PresentFrameTokenGGP #-}
{-# complete (::&) :: PipelineCreationFeedbackCreateInfo #-}
{-# complete (::&) :: SurfaceFullScreenExclusiveInfoEXT #-}
{-# complete (::&) :: SurfaceFullScreenExclusiveWin32InfoEXT #-}
{-# complete (::&) :: SurfaceCapabilitiesFullScreenExclusiveEXT #-}
{-# complete (::&) :: PhysicalDevicePresentBarrierFeaturesNV #-}
{-# complete (::&) :: SurfaceCapabilitiesPresentBarrierNV #-}
{-# complete (::&) :: SwapchainPresentBarrierCreateInfoNV #-}
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
{-# complete (::&) :: PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT #-}
{-# complete (::&) :: AttachmentDescriptionStencilLayout #-}
{-# complete (::&) :: PhysicalDevicePipelineExecutablePropertiesFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceShaderDemoteToHelperInvocationFeatures #-}
{-# complete (::&) :: PhysicalDeviceTexelBufferAlignmentFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceTexelBufferAlignmentProperties #-}
{-# complete (::&) :: PhysicalDeviceSubgroupSizeControlFeatures #-}
{-# complete (::&) :: PhysicalDeviceSubgroupSizeControlProperties #-}
{-# complete (::&) :: PipelineShaderStageRequiredSubgroupSizeCreateInfo #-}
{-# complete (::&) :: SubpassShadingPipelineCreateInfoHUAWEI #-}
{-# complete (::&) :: PhysicalDeviceSubpassShadingPropertiesHUAWEI #-}
{-# complete (::&) :: PhysicalDeviceClusterCullingShaderPropertiesHUAWEI #-}
{-# complete (::&) :: MemoryOpaqueCaptureAddressAllocateInfo #-}
{-# complete (::&) :: PhysicalDeviceLineRasterizationFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceLineRasterizationPropertiesEXT #-}
{-# complete (::&) :: PipelineRasterizationLineStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDevicePipelineCreationCacheControlFeatures #-}
{-# complete (::&) :: PhysicalDeviceVulkan11Features #-}
{-# complete (::&) :: PhysicalDeviceVulkan11Properties #-}
{-# complete (::&) :: PhysicalDeviceVulkan12Features #-}
{-# complete (::&) :: PhysicalDeviceVulkan12Properties #-}
{-# complete (::&) :: PhysicalDeviceVulkan13Features #-}
{-# complete (::&) :: PhysicalDeviceVulkan13Properties #-}
{-# complete (::&) :: PipelineCompilerControlCreateInfoAMD #-}
{-# complete (::&) :: PhysicalDeviceCoherentMemoryFeaturesAMD #-}
{-# complete (::&) :: SamplerCustomBorderColorCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceCustomBorderColorPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceCustomBorderColorFeaturesEXT #-}
{-# complete (::&) :: SamplerBorderColorComponentMappingCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceBorderColorSwizzleFeaturesEXT #-}
{-# complete (::&) :: PipelineLibraryCreateInfoKHR #-}
{-# complete (::&) :: PhysicalDeviceExtendedDynamicStateFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceExtendedDynamicState2FeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceExtendedDynamicState3FeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceExtendedDynamicState3PropertiesEXT #-}
{-# complete (::&) :: RenderPassTransformBeginInfoQCOM #-}
{-# complete (::&) :: CopyCommandTransformInfoQCOM #-}
{-# complete (::&) :: CommandBufferInheritanceRenderPassTransformInfoQCOM #-}
{-# complete (::&) :: PhysicalDeviceDiagnosticsConfigFeaturesNV #-}
{-# complete (::&) :: DeviceDiagnosticsConfigCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures #-}
{-# complete (::&) :: PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceRobustness2FeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceRobustness2PropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceImageRobustnessFeatures #-}
{-# complete (::&) :: PhysicalDeviceWorkgroupMemoryExplicitLayoutFeaturesKHR #-}
{-# complete (::&) :: PhysicalDevicePortabilitySubsetFeaturesKHR #-}
{-# complete (::&) :: PhysicalDevicePortabilitySubsetPropertiesKHR #-}
{-# complete (::&) :: PhysicalDevice4444FormatsFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceSubpassShadingFeaturesHUAWEI #-}
{-# complete (::&) :: PhysicalDeviceClusterCullingShaderFeaturesHUAWEI #-}
{-# complete (::&) :: PhysicalDeviceClusterCullingShaderVrsFeaturesHUAWEI #-}
{-# complete (::&) :: PhysicalDeviceShaderImageAtomicInt64FeaturesEXT #-}
{-# complete (::&) :: FragmentShadingRateAttachmentInfoKHR #-}
{-# complete (::&) :: PipelineFragmentShadingRateStateCreateInfoKHR #-}
{-# complete (::&) :: PhysicalDeviceFragmentShadingRateFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceFragmentShadingRatePropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceShaderTerminateInvocationFeatures #-}
{-# complete (::&) :: PhysicalDeviceFragmentShadingRateEnumsFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceFragmentShadingRateEnumsPropertiesNV #-}
{-# complete (::&) :: PipelineFragmentShadingRateEnumStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceImage2DViewOf3DFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceImageSlicedViewOf3DFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceMutableDescriptorTypeFeaturesEXT #-}
{-# complete (::&) :: MutableDescriptorTypeCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceDepthClipControlFeaturesEXT #-}
{-# complete (::&) :: PipelineViewportDepthClipControlCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceVertexInputDynamicStateFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceExternalMemoryRDMAFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceColorWriteEnableFeaturesEXT #-}
{-# complete (::&) :: PipelineColorWriteCreateInfoEXT #-}
{-# complete (::&) :: MemoryBarrier2 #-}
{-# complete (::&) :: QueueFamilyCheckpointProperties2NV #-}
{-# complete (::&) :: PhysicalDeviceSynchronization2Features #-}
{-# complete (::&) :: PhysicalDeviceHostImageCopyFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceHostImageCopyPropertiesEXT #-}
{-# complete (::&) :: SubresourceHostMemcpySizeEXT #-}
{-# complete (::&) :: HostImageCopyDevicePerformanceQueryEXT #-}
{-# complete (::&) :: PhysicalDevicePrimitivesGeneratedQueryFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceLegacyDitheringFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceMultisampledRenderToSingleSampledFeaturesEXT #-}
{-# complete (::&) :: SubpassResolvePerformanceQueryEXT #-}
{-# complete (::&) :: MultisampledRenderToSingleSampledInfoEXT #-}
{-# complete (::&) :: PhysicalDevicePipelineProtectedAccessFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceInheritedViewportScissorFeaturesNV #-}
{-# complete (::&) :: CommandBufferInheritanceViewportScissorInfoNV #-}
{-# complete (::&) :: PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceProvokingVertexFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceProvokingVertexPropertiesEXT #-}
{-# complete (::&) :: PipelineRasterizationProvokingVertexStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceDescriptorBufferFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceDescriptorBufferPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceDescriptorBufferDensityMapPropertiesEXT #-}
{-# complete (::&) :: DescriptorBufferBindingPushDescriptorBufferHandleEXT #-}
{-# complete (::&) :: OpaqueCaptureDescriptorDataCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderIntegerDotProductFeatures #-}
{-# complete (::&) :: PhysicalDeviceShaderIntegerDotProductProperties #-}
{-# complete (::&) :: PhysicalDeviceDrmPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentShaderBarycentricFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceFragmentShaderBarycentricPropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceRayTracingMotionBlurFeaturesNV #-}
{-# complete (::&) :: AccelerationStructureGeometryMotionTrianglesDataNV #-}
{-# complete (::&) :: AccelerationStructureMotionInfoNV #-}
{-# complete (::&) :: ImportMemoryBufferCollectionFUCHSIA #-}
{-# complete (::&) :: BufferCollectionImageCreateInfoFUCHSIA #-}
{-# complete (::&) :: BufferCollectionBufferCreateInfoFUCHSIA #-}
{-# complete (::&) :: PhysicalDeviceRGBA10X6FormatsFeaturesEXT #-}
{-# complete (::&) :: FormatProperties3 #-}
{-# complete (::&) :: DrmFormatModifierPropertiesList2EXT #-}
{-# complete (::&) :: AndroidHardwareBufferFormatProperties2ANDROID #-}
{-# complete (::&) :: PipelineRenderingCreateInfo #-}
{-# complete (::&) :: RenderingFragmentShadingRateAttachmentInfoKHR #-}
{-# complete (::&) :: RenderingFragmentDensityMapAttachmentInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceDynamicRenderingFeatures #-}
{-# complete (::&) :: CommandBufferInheritanceRenderingInfo #-}
{-# complete (::&) :: AttachmentSampleCountInfoAMD #-}
{-# complete (::&) :: MultiviewPerViewAttributesInfoNVX #-}
{-# complete (::&) :: PhysicalDeviceImageViewMinLodFeaturesEXT #-}
{-# complete (::&) :: ImageViewMinLodCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceLinearColorAttachmentFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT #-}
{-# complete (::&) :: GraphicsPipelineLibraryCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceDescriptorSetHostMappingFeaturesVALVE #-}
{-# complete (::&) :: PhysicalDeviceNestedCommandBufferFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceNestedCommandBufferPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderModuleIdentifierFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderModuleIdentifierPropertiesEXT #-}
{-# complete (::&) :: PipelineShaderStageModuleIdentifierCreateInfoEXT #-}
{-# complete (::&) :: ImageCompressionControlEXT #-}
{-# complete (::&) :: PhysicalDeviceImageCompressionControlFeaturesEXT #-}
{-# complete (::&) :: ImageCompressionPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT #-}
{-# complete (::&) :: RenderPassCreationControlEXT #-}
{-# complete (::&) :: RenderPassCreationFeedbackCreateInfoEXT #-}
{-# complete (::&) :: RenderPassSubpassFeedbackCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceSubpassMergeFeedbackFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceOpacityMicromapFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceOpacityMicromapPropertiesEXT #-}
{-# complete (::&) :: AccelerationStructureTrianglesOpacityMicromapEXT #-}
{-# complete (::&) :: PhysicalDeviceDisplacementMicromapFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceDisplacementMicromapPropertiesNV #-}
{-# complete (::&) :: AccelerationStructureTrianglesDisplacementMicromapNV #-}
{-# complete (::&) :: PhysicalDevicePipelinePropertiesFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderEarlyAndLateFragmentTestsFeaturesAMD #-}
{-# complete (::&) :: ExternalMemoryAcquireUnmodifiedEXT #-}
{-# complete (::&) :: ExportMetalObjectCreateInfoEXT #-}
{-# complete (::&) :: ExportMetalDeviceInfoEXT #-}
{-# complete (::&) :: ExportMetalCommandQueueInfoEXT #-}
{-# complete (::&) :: ExportMetalBufferInfoEXT #-}
{-# complete (::&) :: ImportMetalBufferInfoEXT #-}
{-# complete (::&) :: ExportMetalTextureInfoEXT #-}
{-# complete (::&) :: ImportMetalTextureInfoEXT #-}
{-# complete (::&) :: ExportMetalIOSurfaceInfoEXT #-}
{-# complete (::&) :: ImportMetalIOSurfaceInfoEXT #-}
{-# complete (::&) :: ExportMetalSharedEventInfoEXT #-}
{-# complete (::&) :: ImportMetalSharedEventInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceNonSeamlessCubeMapFeaturesEXT #-}
{-# complete (::&) :: PhysicalDevicePipelineRobustnessFeaturesEXT #-}
{-# complete (::&) :: PipelineRobustnessCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDevicePipelineRobustnessPropertiesEXT #-}
{-# complete (::&) :: ImageViewSampleWeightCreateInfoQCOM #-}
{-# complete (::&) :: PhysicalDeviceImageProcessingFeaturesQCOM #-}
{-# complete (::&) :: PhysicalDeviceImageProcessingPropertiesQCOM #-}
{-# complete (::&) :: PhysicalDeviceTilePropertiesFeaturesQCOM #-}
{-# complete (::&) :: PhysicalDeviceAmigoProfilingFeaturesSEC #-}
{-# complete (::&) :: AmigoProfilingSubmitInfoSEC #-}
{-# complete (::&) :: PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceDepthClampZeroOneFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceAddressBindingReportFeaturesEXT #-}
{-# complete (::&) :: DeviceAddressBindingCallbackDataEXT #-}
{-# complete (::&) :: PhysicalDeviceOpticalFlowFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceOpticalFlowPropertiesNV #-}
{-# complete (::&) :: OpticalFlowImageFormatInfoNV #-}
{-# complete (::&) :: OpticalFlowSessionCreatePrivateDataInfoNV #-}
{-# complete (::&) :: PhysicalDeviceFaultFeaturesEXT #-}
{-# complete (::&) :: PhysicalDevicePipelineLibraryGroupHandlesFeaturesEXT #-}
{-# complete (::&) :: DepthBiasRepresentationInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderCoreBuiltinsPropertiesARM #-}
{-# complete (::&) :: PhysicalDeviceShaderCoreBuiltinsFeaturesARM #-}
{-# complete (::&) :: FrameBoundaryEXT #-}
{-# complete (::&) :: PhysicalDeviceFrameBoundaryFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT #-}
{-# complete (::&) :: SurfacePresentModeEXT #-}
{-# complete (::&) :: SurfacePresentScalingCapabilitiesEXT #-}
{-# complete (::&) :: SurfacePresentModeCompatibilityEXT #-}
{-# complete (::&) :: PhysicalDeviceSwapchainMaintenance1FeaturesEXT #-}
{-# complete (::&) :: SwapchainPresentFenceInfoEXT #-}
{-# complete (::&) :: SwapchainPresentModesCreateInfoEXT #-}
{-# complete (::&) :: SwapchainPresentModeInfoEXT #-}
{-# complete (::&) :: SwapchainPresentScalingCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceDepthBiasControlFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceRayTracingInvocationReorderFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceRayTracingInvocationReorderPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceExtendedSparseAddressSpaceFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceExtendedSparseAddressSpacePropertiesNV #-}
{-# complete (::&) :: DirectDriverLoadingListLUNARG #-}
{-# complete (::&) :: PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM #-}
{-# complete (::&) :: PhysicalDeviceRayTracingPositionFetchFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceShaderCorePropertiesARM #-}
{-# complete (::&) :: PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM #-}
{-# complete (::&) :: MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM #-}
{-# complete (::&) :: QueryLowLatencySupportNV #-}
{-# complete (::&) :: PhysicalDeviceShaderObjectFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderObjectPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderTileImageFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderTileImagePropertiesEXT #-}
{-# complete (::&) :: ImportScreenBufferInfoQNX #-}
{-# complete (::&) :: ScreenBufferFormatPropertiesQNX #-}
{-# complete (::&) :: ExternalFormatQNX #-}
{-# complete (::&) :: PhysicalDeviceExternalMemoryScreenBufferFeaturesQNX #-}
{-# complete (::&) :: PhysicalDeviceCooperativeMatrixFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceCooperativeMatrixPropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceShaderEnqueuePropertiesAMDX #-}
{-# complete (::&) :: PhysicalDeviceShaderEnqueueFeaturesAMDX #-}
{-# complete (::&) :: PipelineShaderStageNodeCreateInfoAMDX #-}
{-# complete (::&) :: PhysicalDeviceCubicClampFeaturesQCOM #-}
{-# complete (::&) :: PhysicalDeviceYcbcrDegammaFeaturesQCOM #-}
{-# complete (::&) :: SamplerYcbcrConversionYcbcrDegammaCreateInfoQCOM #-}
{-# complete (::&) :: PhysicalDeviceCubicWeightsFeaturesQCOM #-}
{-# complete (::&) :: SamplerCubicWeightsCreateInfoQCOM #-}
{-# complete (::&) :: BlitImageCubicWeightsInfoQCOM #-}
{-# complete (::&) :: PhysicalDeviceImageProcessing2FeaturesQCOM #-}
{-# complete (::&) :: PhysicalDeviceImageProcessing2PropertiesQCOM #-}
{-# complete (::&) :: SamplerBlockMatchWindowCreateInfoQCOM #-}
{-# complete (::&) :: PhysicalDeviceDescriptorPoolOverallocationFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceLayeredDriverPropertiesMSFT #-}
{-# complete (::&) :: PhysicalDeviceExternalFormatResolveFeaturesANDROID #-}
{-# complete (::&) :: PhysicalDeviceExternalFormatResolvePropertiesANDROID #-}
{-# complete (::&) :: AndroidHardwareBufferFormatResolvePropertiesANDROID #-}
{-# complete (::&) :: LatencySubmissionPresentIdNV #-}
{-# complete (::&) :: SwapchainLatencyCreateInfoNV #-}
{-# complete (::&) :: LatencySurfaceCapabilitiesNV #-}
{-# complete (::&) :: PhysicalDeviceCudaKernelLaunchFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceCudaKernelLaunchPropertiesNV #-}
{-# complete (::&) :: DeviceQueueShaderCoreControlCreateInfoARM #-}
{-# complete (::&) :: PhysicalDeviceSchedulingControlsFeaturesARM #-}
{-# complete (::&) :: PhysicalDeviceSchedulingControlsPropertiesARM #-}
{-# complete (::&) :: PhysicalDeviceRelaxedLineRasterizationFeaturesIMG #-}
{-# complete (::&) :: PhysicalDeviceRenderPassStripedFeaturesARM #-}
{-# complete (::&) :: PhysicalDeviceRenderPassStripedPropertiesARM #-}
{-# complete (::&) :: RenderPassStripeBeginInfoARM #-}
{-# complete (::&) :: RenderPassStripeSubmitInfoARM #-}

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

