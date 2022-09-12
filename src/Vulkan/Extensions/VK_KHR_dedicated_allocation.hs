{-# language CPP #-}
-- | = Name
--
-- VK_KHR_dedicated_allocation - device extension
--
-- == VK_KHR_dedicated_allocation
--
-- [__Name String__]
--     @VK_KHR_dedicated_allocation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     128
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_memory_requirements2@ to be enabled for any
--         device-level functionality
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_dedicated_allocation] @cubanismo%0A*Here describe the issue or question you have about the VK_KHR_dedicated_allocation extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- This extension enables resources to be bound to a dedicated allocation,
-- rather than suballocated. For any particular resource, applications
-- /can/ query whether a dedicated allocation is recommended, in which case
-- using a dedicated allocation /may/ improve the performance of access to
-- that resource. Normal device memory allocations must support multiple
-- resources per allocation, memory aliasing and sparse binding, which
-- could interfere with some optimizations. Applications should query the
-- implementation for when a dedicated allocation /may/ be beneficial by
-- adding a 'MemoryDedicatedRequirementsKHR' structure to the @pNext@ chain
-- of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a call to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2'
-- or
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'.
-- Certain external handle types and external images or buffers /may/ also
-- depend on dedicated allocations on implementations that associate image
-- or buffer metadata with OS-level memory objects.
--
-- This extension adds a two small structures to memory requirements
-- querying and memory allocation: a new structure that flags whether an
-- image\/buffer should have a dedicated allocation, and a structure
-- indicating the image or buffer that an allocation will be bound to.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'MemoryDedicatedAllocateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2':
--
--     -   'MemoryDedicatedRequirementsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEDICATED_ALLOCATION_EXTENSION_NAME'
--
-- -   'KHR_DEDICATED_ALLOCATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR'
--
-- == Examples
--
-- >     // Create an image with a dedicated allocation based on the
-- >     // implementation's preference
-- >
-- >     VkImageCreateInfo imageCreateInfo =
-- >     {
-- >         // Image creation parameters
-- >     };
-- >
-- >     VkImage image;
-- >     VkResult result = vkCreateImage(
-- >         device,
-- >         &imageCreateInfo,
-- >         NULL,                               // pAllocator
-- >         &image);
-- >
-- >     VkMemoryDedicatedRequirementsKHR dedicatedRequirements =
-- >     {
-- >         VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR,
-- >         NULL,                               // pNext
-- >     };
-- >
-- >     VkMemoryRequirements2 memoryRequirements =
-- >     {
-- >         VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
-- >         &dedicatedRequirements,             // pNext
-- >     };
-- >
-- >     const VkImageMemoryRequirementsInfo2 imageRequirementsInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
-- >         NULL,                               // pNext
-- >         image
-- >     };
-- >
-- >     vkGetImageMemoryRequirements2(
-- >         device,
-- >         &imageRequirementsInfo,
-- >         &memoryRequirements);
-- >
-- >     if (dedicatedRequirements.prefersDedicatedAllocation) {
-- >         // Allocate memory with VkMemoryDedicatedAllocateInfoKHR::image
-- >         // pointing to the image we are allocating the memory for
-- >
-- >         VkMemoryDedicatedAllocateInfoKHR dedicatedInfo =
-- >         {
-- >             VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR,   // sType
-- >             NULL,                                                   // pNext
-- >             image,                                                  // image
-- >             VK_NULL_HANDLE,                                         // buffer
-- >         };
-- >
-- >         VkMemoryAllocateInfo memoryAllocateInfo =
-- >         {
-- >             VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,                 // sType
-- >             &dedicatedInfo,                                         // pNext
-- >             memoryRequirements.size,                                // allocationSize
-- >             FindMemoryTypeIndex(memoryRequirements.memoryTypeBits), // memoryTypeIndex
-- >         };
-- >
-- >         VkDeviceMemory memory;
-- >         vkAllocateMemory(
-- >             device,
-- >             &memoryAllocateInfo,
-- >             NULL,                       // pAllocator
-- >             &memory);
-- >
-- >         // Bind the image to the memory
-- >
-- >         vkBindImageMemory(
-- >             device,
-- >             image,
-- >             memory,
-- >             0);
-- >     } else {
-- >         // Take the normal memory sub-allocation path
-- >     }
--
-- == Version History
--
-- -   Revision 1, 2017-02-27 (James Jones)
--
--     -   Copy content from VK_NV_dedicated_allocation
--
--     -   Add some references to external object interactions to the
--         overview.
--
-- -   Revision 2, 2017-03-27 (Jason Ekstrand)
--
--     -   Rework the extension to be query-based
--
-- -   Revision 3, 2017-07-31 (Jason Ekstrand)
--
--     -   Clarify that memory objects allocated with
--         VkMemoryDedicatedAllocateInfoKHR can only have the specified
--         resource bound and no others.
--
-- == See Also
--
-- 'MemoryDedicatedAllocateInfoKHR', 'MemoryDedicatedRequirementsKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_dedicated_allocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_dedicated_allocation  ( pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR
                                                      , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR
                                                      , MemoryDedicatedRequirementsKHR
                                                      , MemoryDedicatedAllocateInfoKHR
                                                      , KHR_DEDICATED_ALLOCATION_SPEC_VERSION
                                                      , pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION
                                                      , KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
                                                      , pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedAllocateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedRequirements)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS_KHR = STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO_KHR = STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO


-- No documentation found for TopLevel "VkMemoryDedicatedRequirementsKHR"
type MemoryDedicatedRequirementsKHR = MemoryDedicatedRequirements


-- No documentation found for TopLevel "VkMemoryDedicatedAllocateInfoKHR"
type MemoryDedicatedAllocateInfoKHR = MemoryDedicatedAllocateInfo


type KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEDICATED_ALLOCATION_SPEC_VERSION = 3


type KHR_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_KHR_dedicated_allocation"

-- No documentation found for TopLevel "VK_KHR_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_KHR_dedicated_allocation"

