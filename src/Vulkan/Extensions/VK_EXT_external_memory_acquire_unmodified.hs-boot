{-# language CPP #-}
-- | = Name
--
-- VK_EXT_external_memory_acquire_unmodified - device extension
--
-- == VK_EXT_external_memory_acquire_unmodified
--
-- [__Name String__]
--     @VK_EXT_external_memory_acquire_unmodified@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     454
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--
-- [__Contact__]
--
--     -   Lina Versace
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_external_memory_acquire_unmodified] @versalinyaa%0A*Here describe the issue or question you have about the VK_EXT_external_memory_acquire_unmodified extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_external_memory_acquire_unmodified.adoc VK_EXT_external_memory_acquire_unmodified>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-09
--
-- [__Contributors__]
--
--     -   Lina Versace, Google
--
--     -   Chia-I Wu, Google
--
--     -   James Jones, NVIDIA
--
--     -   Yiwei Zhang, Google
--
-- == Description
--
-- A memory barrier /may/ have a performance penalty when acquiring
-- ownership of a subresource range from an external queue family. This
-- extension provides API that /may/ reduce the performance penalty if
-- ownership of the subresource range was previously released to the
-- external queue family and if the resource’s memory has remained
-- unmodified between the release and acquire operations.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.BufferMemoryBarrier2',
--     'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.ImageMemoryBarrier2':
--
--     -   'ExternalMemoryAcquireUnmodifiedEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXTENSION_NAME'
--
-- -   'EXT_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_ACQUIRE_UNMODIFIED_EXT'
--
-- == Version History
--
-- -   Revision 1, 2023-03-09 (Lina Versace)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ExternalMemoryAcquireUnmodifiedEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_external_memory_acquire_unmodified Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_external_memory_acquire_unmodified  (ExternalMemoryAcquireUnmodifiedEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExternalMemoryAcquireUnmodifiedEXT

instance ToCStruct ExternalMemoryAcquireUnmodifiedEXT
instance Show ExternalMemoryAcquireUnmodifiedEXT

instance FromCStruct ExternalMemoryAcquireUnmodifiedEXT

