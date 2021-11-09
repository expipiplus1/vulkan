{-# language CPP #-}
-- | = Name
--
-- VK_KHR_get_memory_requirements2 - device extension
--
-- == VK_KHR_get_memory_requirements2
--
-- [__Name String__]
--     @VK_KHR_get_memory_requirements2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     147
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Jason Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_get_memory_requirements2] @jekstrand%0A<<Here describe the issue or question you have about the VK_KHR_get_memory_requirements2 extension>> >
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
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Hall, Google
--
-- == Description
--
-- This extension provides new entry points to query memory requirements of
-- images and buffers in a way that can be easily extended by other
-- extensions, without introducing any further entry points. The Vulkan 1.0
-- 'Vulkan.Core10.MemoryManagement.MemoryRequirements' and
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements'
-- structures do not include @sType@ and @pNext@ members. This extension
-- wraps them in new structures with these members, so an application can
-- query a chain of memory requirements structures by constructing the
-- chain and letting the implementation fill them in. A new command is
-- added for each @vkGet*MemoryRequrements@ command in core Vulkan 1.0.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'getBufferMemoryRequirements2KHR'
--
-- -   'getImageMemoryRequirements2KHR'
--
-- -   'getImageSparseMemoryRequirements2KHR'
--
-- == New Structures
--
-- -   'BufferMemoryRequirementsInfo2KHR'
--
-- -   'ImageMemoryRequirementsInfo2KHR'
--
-- -   'ImageSparseMemoryRequirementsInfo2KHR'
--
-- -   'MemoryRequirements2KHR'
--
-- -   'SparseImageMemoryRequirements2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME'
--
-- -   'KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR'
--
--     -   'STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR'
--
--     -   'STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR'
--
-- == Version History
--
-- -   Revision 1, 2017-03-23 (Jason Ekstrand)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'BufferMemoryRequirementsInfo2KHR', 'ImageMemoryRequirementsInfo2KHR',
-- 'ImageSparseMemoryRequirementsInfo2KHR', 'MemoryRequirements2KHR',
-- 'SparseImageMemoryRequirements2KHR', 'getBufferMemoryRequirements2KHR',
-- 'getImageMemoryRequirements2KHR', 'getImageSparseMemoryRequirements2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_memory_requirements2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_get_memory_requirements2  ( pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR
                                                          , pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR
                                                          , pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR
                                                          , pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR
                                                          , pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR
                                                          , getBufferMemoryRequirements2KHR
                                                          , getImageMemoryRequirements2KHR
                                                          , getImageSparseMemoryRequirements2KHR
                                                          , BufferMemoryRequirementsInfo2KHR
                                                          , ImageMemoryRequirementsInfo2KHR
                                                          , ImageSparseMemoryRequirementsInfo2KHR
                                                          , MemoryRequirements2KHR
                                                          , SparseImageMemoryRequirements2KHR
                                                          , KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION
                                                          , pattern KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION
                                                          , KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
                                                          , pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (getBufferMemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (getImageMemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (getImageSparseMemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (BufferMemoryRequirementsInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageMemoryRequirementsInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageSparseMemoryRequirementsInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (SparseImageMemoryRequirements2)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR"
pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2_KHR = STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2_KHR = STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2_KHR = STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR"
pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2_KHR = STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR"
pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2_KHR = STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2


-- No documentation found for TopLevel "vkGetBufferMemoryRequirements2KHR"
getBufferMemoryRequirements2KHR = getBufferMemoryRequirements2


-- No documentation found for TopLevel "vkGetImageMemoryRequirements2KHR"
getImageMemoryRequirements2KHR = getImageMemoryRequirements2


-- No documentation found for TopLevel "vkGetImageSparseMemoryRequirements2KHR"
getImageSparseMemoryRequirements2KHR = getImageSparseMemoryRequirements2


-- No documentation found for TopLevel "VkBufferMemoryRequirementsInfo2KHR"
type BufferMemoryRequirementsInfo2KHR = BufferMemoryRequirementsInfo2


-- No documentation found for TopLevel "VkImageMemoryRequirementsInfo2KHR"
type ImageMemoryRequirementsInfo2KHR = ImageMemoryRequirementsInfo2


-- No documentation found for TopLevel "VkImageSparseMemoryRequirementsInfo2KHR"
type ImageSparseMemoryRequirementsInfo2KHR = ImageSparseMemoryRequirementsInfo2


-- No documentation found for TopLevel "VkMemoryRequirements2KHR"
type MemoryRequirements2KHR = MemoryRequirements2


-- No documentation found for TopLevel "VkSparseImageMemoryRequirements2KHR"
type SparseImageMemoryRequirements2KHR = SparseImageMemoryRequirements2


type KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION"
pattern KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_MEMORY_REQUIREMENTS_2_SPEC_VERSION = 1


type KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME = "VK_KHR_get_memory_requirements2"

-- No documentation found for TopLevel "VK_KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME"
pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME = "VK_KHR_get_memory_requirements2"

