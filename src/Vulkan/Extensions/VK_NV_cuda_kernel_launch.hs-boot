{-# language CPP #-}
-- | = Name
--
-- VK_NV_cuda_kernel_launch - device extension
--
-- == VK_NV_cuda_kernel_launch
--
-- [__Name String__]
--     @VK_NV_cuda_kernel_launch@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     308
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_debug_report
--
-- [__Contact__]
--
--     -   Tristan Lorach
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cuda_kernel_launch] @tlorach%0A*Here describe the issue or question you have about the VK_NV_cuda_kernel_launch extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-09-30
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- Interoperability between APIs can sometimes create additional overhead
-- depending on the platform used. This extension targets deployment of
-- existing CUDA kernels via Vulkan, with a way to directly upload PTX
-- kernels and dispatch the kernels from Vulkan’s command buffer without
-- the need to use interoperability between the Vulkan and CUDA contexts.
-- However, we do encourage actual development using the native CUDA
-- runtime for the purpose of debugging and profiling.
--
-- The application will first have to create a CUDA module using
-- 'createCudaModuleNV' then create the CUDA function entry point with
-- 'createCudaFunctionNV'.
--
-- Then in order to dispatch this function, the application will create a
-- command buffer where it will launch the kernel with
-- 'cmdCudaLaunchKernelNV'.
--
-- When done, the application will then destroy the function handle, as
-- well as the CUDA module handle with 'destroyCudaFunctionNV' and
-- 'destroyCudaModuleNV'.
--
-- To reduce the impact of compilation time, this extension offers the
-- capability to return a binary cache from the PTX that was provided. For
-- this, a first query for the required cache size is made with
-- 'getCudaModuleCacheNV' with a @NULL@ pointer to a buffer and with a
-- valid pointer receiving the size; then another call of the same function
-- with a valid pointer to a buffer to retrieve the data. The resulting
-- cache could then be user later for further runs of this application by
-- sending this cache instead of the PTX code (using the same
-- 'createCudaModuleNV'), thus significantly speeding up the initialization
-- of the CUDA module.
--
-- As with 'Vulkan.Core10.Handles.PipelineCache', the binary cache depends
-- on the hardware architecture. The application must assume the cache
-- might fail, and need to handle falling back to the original PTX code as
-- necessary. Most often, the cache will succeed if the same GPU driver and
-- architecture is used between the cache generation from PTX and the use
-- of this cache. In the event of a new driver version, or if using a
-- different GPU architecture, the cache is likely to become invalid.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.CudaFunctionNV'
--
-- -   'Vulkan.Extensions.Handles.CudaModuleNV'
--
-- == New Commands
--
-- -   'cmdCudaLaunchKernelNV'
--
-- -   'createCudaFunctionNV'
--
-- -   'createCudaModuleNV'
--
-- -   'destroyCudaFunctionNV'
--
-- -   'destroyCudaModuleNV'
--
-- -   'getCudaModuleCacheNV'
--
-- == New Structures
--
-- -   'CudaFunctionCreateInfoNV'
--
-- -   'CudaLaunchInfoNV'
--
-- -   'CudaModuleCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCudaKernelLaunchFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCudaKernelLaunchPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_CUDA_KERNEL_LAUNCH_EXTENSION_NAME'
--
-- -   'NV_CUDA_KERNEL_LAUNCH_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_CUDA_FUNCTION_NV'
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_CUDA_MODULE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUDA_FUNCTION_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUDA_LAUNCH_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUDA_MODULE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUDA_KERNEL_LAUNCH_PROPERTIES_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_report VK_EXT_debug_report>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_CUDA_FUNCTION_NV_EXT'
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_CUDA_MODULE_NV_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2020-03-01 (Tristan Lorach)
--
-- -   Revision 2, 2020-09-30 (Tristan Lorach)
--
-- == See Also
--
-- 'CudaFunctionCreateInfoNV', 'Vulkan.Extensions.Handles.CudaFunctionNV',
-- 'CudaLaunchInfoNV', 'CudaModuleCreateInfoNV',
-- 'Vulkan.Extensions.Handles.CudaModuleNV',
-- 'PhysicalDeviceCudaKernelLaunchFeaturesNV',
-- 'PhysicalDeviceCudaKernelLaunchPropertiesNV', 'cmdCudaLaunchKernelNV',
-- 'createCudaFunctionNV', 'createCudaModuleNV', 'destroyCudaFunctionNV',
-- 'destroyCudaModuleNV', 'getCudaModuleCacheNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_cuda_kernel_launch Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cuda_kernel_launch  ( CudaFunctionCreateInfoNV
                                                   , CudaLaunchInfoNV
                                                   , CudaModuleCreateInfoNV
                                                   , PhysicalDeviceCudaKernelLaunchFeaturesNV
                                                   , PhysicalDeviceCudaKernelLaunchPropertiesNV
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CudaFunctionCreateInfoNV

instance ToCStruct CudaFunctionCreateInfoNV
instance Show CudaFunctionCreateInfoNV

instance FromCStruct CudaFunctionCreateInfoNV


data CudaLaunchInfoNV

instance ToCStruct CudaLaunchInfoNV
instance Show CudaLaunchInfoNV

instance FromCStruct CudaLaunchInfoNV


data CudaModuleCreateInfoNV

instance ToCStruct CudaModuleCreateInfoNV
instance Show CudaModuleCreateInfoNV

instance FromCStruct CudaModuleCreateInfoNV


data PhysicalDeviceCudaKernelLaunchFeaturesNV

instance ToCStruct PhysicalDeviceCudaKernelLaunchFeaturesNV
instance Show PhysicalDeviceCudaKernelLaunchFeaturesNV

instance FromCStruct PhysicalDeviceCudaKernelLaunchFeaturesNV


data PhysicalDeviceCudaKernelLaunchPropertiesNV

instance ToCStruct PhysicalDeviceCudaKernelLaunchPropertiesNV
instance Show PhysicalDeviceCudaKernelLaunchPropertiesNV

instance FromCStruct PhysicalDeviceCudaKernelLaunchPropertiesNV

