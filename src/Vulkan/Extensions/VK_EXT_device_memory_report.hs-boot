{-# language CPP #-}
-- | = Name
--
-- VK_EXT_device_memory_report - device extension
--
-- == VK_EXT_device_memory_report
--
-- [__Name String__]
--     @VK_EXT_device_memory_report@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     285
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Yiwei Zhang
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_device_memory_report] @zhangyiwei%0A*Here describe the issue or question you have about the VK_EXT_device_memory_report extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-01-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Yiwei Zhang, Google
--
--     -   Jesse Hall, Google
--
-- == Description
--
-- This device extension allows registration of device memory event
-- callbacks upon device creation, so that applications or middleware can
-- obtain detailed information about memory usage and how memory is
-- associated with Vulkan objects. This extension exposes the actual
-- underlying device memory usage, including allocations that are not
-- normally visible to the application, such as memory consumed by
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines'. It is intended
-- primarily for use by debug tooling rather than for production
-- applications.
--
-- == New Structures
--
-- -   'DeviceMemoryReportCallbackDataEXT'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceDeviceMemoryReportCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDeviceMemoryReportFeaturesEXT'
--
-- == New Function Pointers
--
-- -   'PFN_vkDeviceMemoryReportCallbackEXT'
--
-- == New Enums
--
-- -   'DeviceMemoryReportEventTypeEXT'
--
-- == New Bitmasks
--
-- -   'DeviceMemoryReportFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEVICE_MEMORY_REPORT_EXTENSION_NAME'
--
-- -   'EXT_DEVICE_MEMORY_REPORT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_MEMORY_REPORT_CALLBACK_DATA_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should this be better expressed as an extension to VK_EXT_debug_utils
-- and its general-purpose messenger construct?
--
-- __RESOLVED__: No. The intended lifecycle is quite different. We want to
-- make this extension tied to the device’s lifecycle. Each ICD just
-- handles its own implementation of this extension, and this extension
-- will only be directly exposed from the ICD. So we can avoid the extra
-- implementation complexity used to accommodate the flexibility of
-- @VK_EXT_debug_utils@ extension.
--
-- 2) Can we extend and use the existing internal allocation callbacks
-- instead of adding the new callback structure in this extension?
--
-- __RESOLVED__: No. Our memory reporting layer that combines this
-- information with other memory information it collects directly (e.g.
-- bindings of resources to 'Vulkan.Core10.Handles.DeviceMemory') would
-- have to intercept all entry points that take a
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' parameter and
-- inject its own @pfnInternalAllocation@ and @pfnInternalFree@. That may
-- be doable for the extensions we know about, but not for ones we do not.
-- The proposal would work fine in the face of most unknown extensions. But
-- even for ones we know about, since apps can provide a different set of
-- callbacks and userdata and those can be retained by the driver and used
-- later (esp. for pool object, but not just those), we would have to
-- dynamically allocate the interception trampoline every time. That is
-- getting to be an unreasonably large amount of complexity and (possibly)
-- overhead.
--
-- We are interested in both alloc\/free and import\/unimport. The latter
-- is fairly important for tracking (and avoiding double-counting) of
-- swapchain images (still true with “native swapchains” based on external
-- memory) and media\/camera interop. Though we might be able to handle
-- this with additional
-- 'Vulkan.Core10.Enums.InternalAllocationType.InternalAllocationType'
-- values, for import\/export we do want to be able to tie this to the
-- external resource, which is one thing that the @memoryObjectId@ is for.
--
-- The internal alloc\/free callbacks are not extensible except via new
-- 'Vulkan.Core10.Enums.InternalAllocationType.InternalAllocationType'
-- values. The 'DeviceMemoryReportCallbackDataEXT' in this extension is
-- extensible. That was deliberate: there is a real possibility we will
-- want to get extra information in the future. As one example, currently
-- this reports only physical allocations, but we believe there are
-- interesting cases for tracking how populated that VA region is.
--
-- The callbacks are clearly specified as only callable within the context
-- of a call from the app into Vulkan. We believe there are some cases
-- where drivers can allocate device memory asynchronously. This was one of
-- the sticky issues that derailed the internal device memory allocation
-- reporting design (which is essentially what this extension is trying to
-- do) leading up to 1.0.
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' is described in
-- a section called “Host memory” and the intro to it is very explicitly
-- about host memory. The other callbacks are all inherently about host
-- memory. But this extension is very focused on device memory.
--
-- 3) Should the callback be reporting which heap is used?
--
-- __RESOLVED__: Yes. It is important for non-UMA systems to have all the
-- device memory allocations attributed to the corresponding device memory
-- heaps. For internally-allocated device memory, @heapIndex@ will always
-- correspond to an advertised heap, rather than having a magic value
-- indicating a non-advertised heap. Drivers can advertise heaps that do
-- not have any corresponding memory types if they need to.
--
-- 4) Should we use an array of callback for the layers to intercept
-- instead of chaining multiple of the
-- 'DeviceDeviceMemoryReportCreateInfoEXT' structures in the @pNext@ of
-- 'Vulkan.Core10.Device.DeviceCreateInfo'?
--
-- __RESOLVED__ No. The pointer to the
-- 'DeviceDeviceMemoryReportCreateInfoEXT' structure itself is const and
-- you cannot just cast it away. Thus we cannot update the callback array
-- inside the structure. In addition, we cannot drop this @pNext@ chain
-- either, so making a copy of this whole structure does not work either.
--
-- 5) Should we track bulk allocations shared among multiple objects?
--
-- __RESOLVED__ No. Take the shader heap as an example. Some
-- implementations will let multiple 'Vulkan.Core10.Handles.Pipeline'
-- objects share the same shader heap. We are not asking the implementation
-- to report 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_PIPELINE' along
-- with a 'Vulkan.Core10.APIConstants.NULL_HANDLE' for this bulk
-- allocation. Instead, this bulk allocation is considered as a layer below
-- what this extension is interested in. Later, when the actual
-- 'Vulkan.Core10.Handles.Pipeline' objects are created by suballocating
-- from the bulk allocation, we ask the implementation to report the valid
-- handles of the 'Vulkan.Core10.Handles.Pipeline' objects along with the
-- actual suballocated sizes and different @memoryObjectId@.
--
-- 6) Can we require the callbacks to be always called in the same thread
-- with the Vulkan commands?
--
-- __RESOLVED__ No. Some implementations might choose to multiplex work
-- from multiple application threads into a single backend thread and
-- perform JIT allocations as a part of that flow. Since this behavior is
-- theoretically legit, we cannot require the callbacks to be always called
-- in the same thread with the Vulkan commands, and the note is to remind
-- the applications to handle this case properly.
--
-- 7) Should we add an additional “allocation failed” event type with
-- things like size and heap index reported?
--
-- __RESOLVED__ Yes. This fits in well with the callback infrastructure
-- added in this extension, and implementation touches the same code and
-- has the same overheads as the rest of the extension. It could help
-- debugging things like getting an
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' error when ending
-- a command buffer. Right now the allocation failure could have happened
-- anywhere during recording, and a callback would be really useful to
-- understand where and why.
--
-- == Version History
--
-- -   Revision 1, 2020-08-26 (Yiwei Zhang)
--
--     -   Initial version
--
-- -   Revision 2, 2021-01-06 (Yiwei Zhang)
--
--     -   Minor description update
--
-- == See Also
--
-- 'PFN_vkDeviceMemoryReportCallbackEXT',
-- 'DeviceDeviceMemoryReportCreateInfoEXT',
-- 'DeviceMemoryReportCallbackDataEXT', 'DeviceMemoryReportEventTypeEXT',
-- 'DeviceMemoryReportFlagsEXT',
-- 'PhysicalDeviceDeviceMemoryReportFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_device_memory_report Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_device_memory_report  ( DeviceDeviceMemoryReportCreateInfoEXT
                                                      , DeviceMemoryReportCallbackDataEXT
                                                      , PhysicalDeviceDeviceMemoryReportFeaturesEXT
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceDeviceMemoryReportCreateInfoEXT

instance ToCStruct DeviceDeviceMemoryReportCreateInfoEXT
instance Show DeviceDeviceMemoryReportCreateInfoEXT

instance FromCStruct DeviceDeviceMemoryReportCreateInfoEXT


data DeviceMemoryReportCallbackDataEXT

instance ToCStruct DeviceMemoryReportCallbackDataEXT
instance Show DeviceMemoryReportCallbackDataEXT

instance FromCStruct DeviceMemoryReportCallbackDataEXT


data PhysicalDeviceDeviceMemoryReportFeaturesEXT

instance ToCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT
instance Show PhysicalDeviceDeviceMemoryReportFeaturesEXT

instance FromCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT

