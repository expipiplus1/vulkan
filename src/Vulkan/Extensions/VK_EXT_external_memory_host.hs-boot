{-# language CPP #-}
-- | = Name
--
-- VK_EXT_external_memory_host - device extension
--
-- == VK_EXT_external_memory_host
--
-- [__Name String__]
--     @VK_EXT_external_memory_host@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     179
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_external_memory_host] @drakos-amd%0A*Here describe the issue or question you have about the VK_EXT_external_memory_host extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-11-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jaakko Konttinen, AMD
--
--     -   David Mao, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Tobias Hector, Imagination Technologies
--
--     -   Faith Ekstrand, Intel
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- This extension enables an application to import host allocations and
-- host mapped foreign device memory to Vulkan memory objects.
--
-- == New Commands
--
-- -   'getMemoryHostPointerPropertiesEXT'
--
-- == New Structures
--
-- -   'MemoryHostPointerPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryHostPointerInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExternalMemoryHostPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME'
--
-- -   'EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT'
--
-- == Issues
--
-- 1) What memory type has to be used to import host pointers?
--
-- __RESOLVED__: Depends on the implementation. Applications have to use
-- the new 'getMemoryHostPointerPropertiesEXT' command to query the
-- supported memory types for a particular host pointer. The reported
-- memory types may include memory types that come from a memory heap that
-- is otherwise not usable for regular memory object allocation and thus
-- such a heap’s size may be zero.
--
-- 2) Can the application still access the contents of the host allocation
-- after importing?
--
-- __RESOLVED__: Yes. However, usual synchronization requirements apply.
--
-- 3) Can the application free the host allocation?
--
-- __RESOLVED__: No, it violates valid usage conditions. Using the memory
-- object imported from a host allocation that is already freed thus
-- results in undefined behavior.
--
-- 4) Is 'Vulkan.Core10.Memory.mapMemory' expected to return the same host
-- address which was specified when importing it to the memory object?
--
-- __RESOLVED__: No. Implementations are allowed to return the same address
-- but it is not required. Some implementations might return a different
-- virtual mapping of the allocation, although the same physical pages will
-- be used.
--
-- 5) Is there any limitation on the alignment of the host pointer and\/or
-- size?
--
-- __RESOLVED__: Yes. Both the address and the size have to be an integer
-- multiple of @minImportedHostPointerAlignment@. In addition, some
-- platforms and foreign devices may have additional restrictions.
--
-- 6) Can the same host allocation be imported multiple times into a given
-- physical device?
--
-- __RESOLVED__: No, at least not guaranteed by this extension. Some
-- platforms do not allow locking the same physical pages for device access
-- multiple times, so attempting to do it may result in undefined behavior.
--
-- 7) Does this extension support exporting the new handle type?
--
-- __RESOLVED__: No.
--
-- 8) Should we include the possibility to import host mapped foreign
-- device memory using this API?
--
-- __RESOLVED__: Yes, through a separate handle type. Implementations are
-- still allowed to support only one of the handle types introduced by this
-- extension by not returning import support for a particular handle type
-- as returned in
-- 'Vulkan.Extensions.VK_KHR_external_memory_capabilities.ExternalMemoryPropertiesKHR'.
--
-- == Version History
--
-- -   Revision 1, 2017-11-10 (Daniel Rakos)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'ImportMemoryHostPointerInfoEXT', 'MemoryHostPointerPropertiesEXT',
-- 'PhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'getMemoryHostPointerPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_external_memory_host Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_external_memory_host  ( ImportMemoryHostPointerInfoEXT
                                                      , MemoryHostPointerPropertiesEXT
                                                      , PhysicalDeviceExternalMemoryHostPropertiesEXT
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImportMemoryHostPointerInfoEXT

instance ToCStruct ImportMemoryHostPointerInfoEXT
instance Show ImportMemoryHostPointerInfoEXT

instance FromCStruct ImportMemoryHostPointerInfoEXT


data MemoryHostPointerPropertiesEXT

instance ToCStruct MemoryHostPointerPropertiesEXT
instance Show MemoryHostPointerPropertiesEXT

instance FromCStruct MemoryHostPointerPropertiesEXT


data PhysicalDeviceExternalMemoryHostPropertiesEXT

instance ToCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT
instance Show PhysicalDeviceExternalMemoryHostPropertiesEXT

instance FromCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT

