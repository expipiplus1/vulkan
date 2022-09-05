{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_memory - device extension
--
-- == VK_KHR_external_memory
--
-- [__Name String__]
--     @VK_KHR_external_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     73
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory_capabilities@ to be enabled for
--         any device-level functionality
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_memory] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_external_memory extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-20
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_dedicated_allocation@.
--
--     -   Interacts with @VK_NV_dedicated_allocation@.
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jason Ekstrand, Intel
--
--     -   Ian Elliot, Google
--
--     -   Jesse Hall, Google
--
--     -   Tobias Hector, Imagination Technologies
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Daniel Rakos, AMD
--
--     -   Carsten Rohde, NVIDIA
--
--     -   Ray Smith, ARM
--
--     -   Chad Versace, Google
--
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- logical devices or instances, in multiple processes, and\/or in multiple
-- APIs. This extension enables an application to export non-Vulkan handles
-- from Vulkan memory objects such that the underlying resources can be
-- referenced outside the scope of the Vulkan logical device that created
-- them.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo':
--
--     -   'ExternalMemoryBufferCreateInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ExternalMemoryImageCreateInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ExportMemoryAllocateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_MEMORY_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_MEMORY_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'ERROR_INVALID_EXTERNAL_HANDLE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) How do applications correlate two physical devices across process or
-- Vulkan instance boundaries?
--
-- __RESOLVED__: New device ID fields have been introduced by
-- @VK_KHR_external_memory_capabilities@. These fields, combined with the
-- existing
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@driverVersion@
-- field can be used to identify compatible devices across processes,
-- drivers, and APIs.
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@pipelineCacheUUID@
-- is not sufficient for this purpose because despite its description in
-- the specification, it need only identify a unique pipeline cache format
-- in practice. Multiple devices may be able to use the same pipeline cache
-- data, and hence it would be desirable for all of them to have the same
-- pipeline cache UUID. However, only the same concrete physical device can
-- be used when sharing memory, so an actual unique device ID was
-- introduced. Further, the pipeline cache UUID was specific to Vulkan, but
-- correlation with other, non-extensible APIs is required to enable
-- interoperation with those APIs.
--
-- 2) If memory objects are shared between processes and APIs, is this
-- considered aliasing according to the rules outlined in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-memory-aliasing Memory Aliasing>
-- section?
--
-- __RESOLVED__: Yes. Applications must take care to obey all restrictions
-- imposed on aliased resources when using memory across multiple Vulkan
-- instances or other APIs.
--
-- 3) Are new image layouts or metadata required to specify image layouts
-- and layout transitions compatible with non-Vulkan APIs, or with other
-- instances of the same Vulkan driver?
--
-- __RESOLVED__: Separate instances of the same Vulkan driver running on
-- the same GPU should have identical internal layout semantics, so
-- applications have the tools they need to ensure views of images are
-- consistent between the two instances. Other APIs will fall into two
-- categories: Those that are Vulkan- compatible, and those that are
-- Vulkan-incompatible. Vulkan-incompatible APIs will require the image to
-- be in the GENERAL layout whenever they are accessing them.
--
-- Note this does not attempt to address cross-device transitions, nor
-- transitions to engines on the same device which are not visible within
-- the Vulkan API. Both of these are beyond the scope of this extension.
--
-- 4) Is a new barrier flag or operation of some type needed to prepare
-- external memory for handoff to another Vulkan instance or API and\/or
-- receive it from another instance or API?
--
-- __RESOLVED__: Yes. Some implementations need to perform additional cache
-- management when transitioning memory between address spaces and other
-- APIs, instances, or processes which may operate in a separate address
-- space. Options for defining this transition include:
--
-- -   A new structure that can be added to the @pNext@ list in
--     'Vulkan.Core10.OtherTypes.MemoryBarrier',
--     'Vulkan.Core10.OtherTypes.BufferMemoryBarrier', and
--     'Vulkan.Core10.OtherTypes.ImageMemoryBarrier'.
--
-- -   A new bit in 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags' that
--     can be set to indicate an “external” access.
--
-- -   A new bit in
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags'
--
-- -   A new special queue family that represents an “external” queue.
--
-- A new structure has the advantage that the type of external transition
-- can be described in as much detail as necessary. However, there is not
-- currently a known need for anything beyond differentiating between
-- external and internal accesses, so this is likely an over-engineered
-- solution. The access flag bit has the advantage that it can be applied
-- at buffer, image, or global granularity, and semantically it maps pretty
-- well to the operation being described. Additionally, the API already
-- includes 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_MEMORY_READ_BIT' and
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_MEMORY_WRITE_BIT' which
-- appear to be intended for this purpose. However, there is no obvious
-- pipeline stage that would correspond to an external access, and
-- therefore no clear way to use
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_MEMORY_READ_BIT' or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_MEMORY_WRITE_BIT'.
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags' and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags' operate
-- at command granularity rather than image or buffer granularity, which
-- would make an entire pipeline barrier an internal→external or
-- external→internal barrier. This may not be a problem in practice, but
-- seems like the wrong scope. Another downside of
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags' is that it
-- lacks inherent directionality: there are no @src@ and @dst@ variants of
-- it in the barrier or dependency description semantics, so two bits might
-- need to be added to describe both internal→external and
-- external→internal transitions. Transitioning a resource to a special
-- queue family corresponds well with the operation of transitioning to a
-- separate Vulkan instance, in that both operations ideally include
-- scheduling a barrier on both sides of the transition: Both the releasing
-- and the acquiring queue or process. Using a special queue family
-- requires adding an additional reserved queue family index. Re-using
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED' would have left it
-- unclear how to transition a concurrent usage resource from one process
-- to another, since the semantics would have likely been equivalent to the
-- currently-ignored transition of
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED' → 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'.
-- Fortunately, creating a new reserved queue family index is not invasive.
--
-- Based on the above analysis, the approach of transitioning to a special
-- “external” queue family was chosen.
--
-- 5) Do internal driver memory arrangements and\/or other internal driver
-- image properties need to be exported and imported when sharing images
-- across processes or APIs.
--
-- __RESOLVED__: Some vendors claim this is necessary on their
-- implementations, but it was determined that the security risks of
-- allowing opaque metadata to be passed from applications to the driver
-- were too high. Therefore, implementations which require metadata will
-- need to associate it with the objects represented by the external
-- handles, and rely on the dedicated allocation mechanism to associate the
-- exported and imported memory objects with a single image or buffer.
--
-- 6) Most prior interoperation and cross-process sharing APIs have been
-- based on image-level sharing. Should Vulkan sharing be based on
-- memory-object sharing or image sharing?
--
-- __RESOLVED__: These extensions have assumed memory-level sharing is the
-- correct granularity. Vulkan is a lower-level API than most prior APIs,
-- and as such attempts to closely align with to the underlying primitives
-- of the hardware and system-level drivers it abstracts. In general, the
-- resource that holds the backing store for both images and buffers of
-- various types is memory. Images and buffers are merely metadata
-- containing brief descriptions of the layout of bits within that memory.
--
-- Because memory object-based sharing is aligned with the overall Vulkan
-- API design, it enables the full range of Vulkan capabilities with
-- external objects. External memory can be used as backing for sparse
-- images, for example, whereas such usage would be awkward at best with a
-- sharing mechanism based on higher-level primitives such as images.
-- Further, aligning the mechanism with the API in this way provides some
-- hope of trivial compatibility with future API enhancements. If new
-- objects backed by memory objects are added to the API, they too can be
-- used across processes with minimal additions to the base external memory
-- APIs.
--
-- Earlier APIs implemented interop at a higher level, and this
-- necessitated entirely separate sharing APIs for images and buffers. To
-- co-exist and interoperate with those APIs, the Vulkan external sharing
-- mechanism must accommodate their model. However, if it can be agreed
-- that memory-based sharing is the more desirable and forward-looking
-- design, legacy interoperation constraints can be considered another
-- reason to favor memory-based sharing: while native and legacy driver
-- primitives that may be used to implement sharing may not be as low-level
-- as the API here suggests, raw memory is still the least common
-- denominator among the types. Image-based sharing can be cleanly derived
-- from a set of base memory- object sharing APIs with minimal effort,
-- whereas image-based sharing does not generalize well to buffer or
-- raw-memory sharing. Therefore, following the general Vulkan design
-- principle of minimalism, it is better to expose interopability with
-- image-based native and external primitives via the memory sharing API,
-- and place sufficient limits on their usage to ensure they can be used
-- only as backing for equivalent Vulkan images. This provides a consistent
-- API for applications regardless of which platform or external API they
-- are targeting, which makes development of multi-API and multi-platform
-- applications simpler.
--
-- 7) Should Vulkan define a common external handle type and provide Vulkan
-- functions to facilitate cross-process sharing of such handles rather
-- than relying on native handles to define the external objects?
--
-- __RESOLVED__: No. Cross-process sharing of resources is best left to
-- native platforms. There are myriad security and extensibility issues
-- with such a mechanism, and attempting to re-solve all those issues
-- within Vulkan does not align with Vulkan’s purpose as a graphics API. If
-- desired, such a mechanism could be built as a layer or helper library on
-- top of the opaque native handle defined in this family of extensions.
--
-- 8) Must implementations provide additional guarantees about state
-- implicitly included in memory objects for those memory objects that may
-- be exported?
--
-- __RESOLVED__: Implementations must ensure that sharing memory objects
-- does not transfer any information between the exporting and importing
-- instances and APIs other than that required to share the data contained
-- in the memory objects explicitly shared. As specific examples, data from
-- previously freed memory objects that used the same underlying physical
-- memory, and data from memory obects using adjacent physical memory must
-- not be visible to applications importing an exported memory object.
--
-- 9) Must implementations validate external handles the application
-- provides as inputs to memory import operations?
--
-- __RESOLVED__: Implementations must return an error to the application if
-- the provided memory handle cannot be used to complete the requested
-- import operation. However, implementations need not validate handles are
-- of the exact type specified by the application.
--
-- == Version History
--
-- -   Revision 1, 2016-10-20 (James Jones)
--
--     -   Initial version
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL_KHR',
-- 'ExportMemoryAllocateInfoKHR', 'ExternalMemoryBufferCreateInfoKHR',
-- 'ExternalMemoryImageCreateInfoKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_external_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_external_memory  ( pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR
                                                 , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR
                                                 , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR
                                                 , pattern ERROR_INVALID_EXTERNAL_HANDLE_KHR
                                                 , pattern QUEUE_FAMILY_EXTERNAL_KHR
                                                 , ExternalMemoryImageCreateInfoKHR
                                                 , ExternalMemoryBufferCreateInfoKHR
                                                 , ExportMemoryAllocateInfoKHR
                                                 , KHR_EXTERNAL_MEMORY_SPEC_VERSION
                                                 , pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION
                                                 , KHR_EXTERNAL_MEMORY_EXTENSION_NAME
                                                 , pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME
                                                 ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExportMemoryAllocateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryBufferCreateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryImageCreateInfo)
import Vulkan.Core10.Enums.Result (Result(ERROR_INVALID_EXTERNAL_HANDLE))
import Vulkan.Core10.APIConstants (pattern QUEUE_FAMILY_EXTERNAL)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR = STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR = STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR = STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO


-- No documentation found for TopLevel "VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR"
pattern ERROR_INVALID_EXTERNAL_HANDLE_KHR = ERROR_INVALID_EXTERNAL_HANDLE


-- No documentation found for TopLevel "VK_QUEUE_FAMILY_EXTERNAL_KHR"
pattern QUEUE_FAMILY_EXTERNAL_KHR = QUEUE_FAMILY_EXTERNAL


-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfoKHR"
type ExternalMemoryImageCreateInfoKHR = ExternalMemoryImageCreateInfo


-- No documentation found for TopLevel "VkExternalMemoryBufferCreateInfoKHR"
type ExternalMemoryBufferCreateInfoKHR = ExternalMemoryBufferCreateInfo


-- No documentation found for TopLevel "VkExportMemoryAllocateInfoKHR"
type ExportMemoryAllocateInfoKHR = ExportMemoryAllocateInfo


type KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_MEMORY_SPEC_VERSION = 1


type KHR_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_KHR_external_memory"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_EXTENSION_NAME = "VK_KHR_external_memory"

