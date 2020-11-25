{-# language CPP #-}
-- | = Name
--
-- VK_NV_dedicated_allocation - device extension
--
-- = Registered Extension Number
--
-- 27
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- = Deprecation state
--
-- -   /Deprecated/ by @VK_KHR_dedicated_allocation@ extension
--
--     -   Which in turn was /promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-05-31
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows device memory to be allocated for a particular
-- buffer or image resource, which on some devices can significantly
-- improve the performance of that resource. Normal device memory
-- allocations must support memory aliasing and sparse binding, which could
-- interfere with optimizations like framebuffer compression or efficient
-- page table usage. This is important for render targets and very large
-- resources, but need not (and probably should not) be used for smaller
-- resources that can benefit from suballocation.
--
-- This extension adds a few small structures to resource creation and
-- memory allocation: a new structure that flags whether am image\/buffer
-- will have a dedicated allocation, and a structure indicating the image
-- or buffer that an allocation will be bound to.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo':
--
--     -   'DedicatedAllocationBufferCreateInfoNV'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'DedicatedAllocationImageCreateInfoNV'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'DedicatedAllocationMemoryAllocateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_DEDICATED_ALLOCATION_EXTENSION_NAME'
--
-- -   'NV_DEDICATED_ALLOCATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV'
--
-- == Examples
--
-- >     // Create an image with
-- >     // VkDedicatedAllocationImageCreateInfoNV::dedicatedAllocation
-- >     // set to VK_TRUE
-- >
-- >     VkDedicatedAllocationImageCreateInfoNV dedicatedImageInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV,            // sType
-- >         NULL,                                                                   // pNext
-- >         VK_TRUE,                                                                // dedicatedAllocation
-- >     };
-- >
-- >     VkImageCreateInfo imageCreateInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO,    // sType
-- >         &dedicatedImageInfo                     // pNext
-- >         // Other members set as usual
-- >     };
-- >
-- >     VkImage image;
-- >     VkResult result = vkCreateImage(
-- >         device,
-- >         &imageCreateInfo,
-- >         NULL,                       // pAllocator
-- >         &image);
-- >
-- >     VkMemoryRequirements memoryRequirements;
-- >     vkGetImageMemoryRequirements(
-- >         device,
-- >         image,
-- >         &memoryRequirements);
-- >
-- >     // Allocate memory with VkDedicatedAllocationMemoryAllocateInfoNV::image
-- >     // pointing to the image we are allocating the memory for
-- >
-- >     VkDedicatedAllocationMemoryAllocateInfoNV dedicatedInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV,             // sType
-- >         NULL,                                                                       // pNext
-- >         image,                                                                      // image
-- >         VK_NULL_HANDLE,                                                             // buffer
-- >     };
-- >
-- >     VkMemoryAllocateInfo memoryAllocateInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,                 // sType
-- >         &dedicatedInfo,                                         // pNext
-- >         memoryRequirements.size,                                // allocationSize
-- >         FindMemoryTypeIndex(memoryRequirements.memoryTypeBits), // memoryTypeIndex
-- >     };
-- >
-- >     VkDeviceMemory memory;
-- >     vkAllocateMemory(
-- >         device,
-- >         &memoryAllocateInfo,
-- >         NULL,                       // pAllocator
-- >         &memory);
-- >
-- >     // Bind the image to the memory
-- >
-- >     vkBindImageMemory(
-- >         device,
-- >         image,
-- >         memory,
-- >         0);
--
-- == Version History
--
-- -   Revision 1, 2016-05-31 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DedicatedAllocationBufferCreateInfoNV',
-- 'DedicatedAllocationImageCreateInfoNV',
-- 'DedicatedAllocationMemoryAllocateInfoNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_dedicated_allocation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_dedicated_allocation  ( DedicatedAllocationBufferCreateInfoNV
                                                     , DedicatedAllocationImageCreateInfoNV
                                                     , DedicatedAllocationMemoryAllocateInfoNV
                                                     ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DedicatedAllocationBufferCreateInfoNV

instance ToCStruct DedicatedAllocationBufferCreateInfoNV
instance Show DedicatedAllocationBufferCreateInfoNV

instance FromCStruct DedicatedAllocationBufferCreateInfoNV


data DedicatedAllocationImageCreateInfoNV

instance ToCStruct DedicatedAllocationImageCreateInfoNV
instance Show DedicatedAllocationImageCreateInfoNV

instance FromCStruct DedicatedAllocationImageCreateInfoNV


data DedicatedAllocationMemoryAllocateInfoNV

instance ToCStruct DedicatedAllocationMemoryAllocateInfoNV
instance Show DedicatedAllocationMemoryAllocateInfoNV

instance FromCStruct DedicatedAllocationMemoryAllocateInfoNV

