{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_memory - device extension
--
-- == VK_NV_external_memory
--
-- [__Name String__]
--     @VK_NV_external_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     57
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_capabilities VK_NV_external_memory_capabilities>
--
-- [__Deprecation State__]
--
--     -   /Deprecated/ by @VK_KHR_external_memory@ extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_external_memory] @cubanismo%0A*Here describe the issue or question you have about the VK_NV_external_memory extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-08-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- Applications may wish to export memory to other Vulkan instances or
-- other APIs, or import memory from other Vulkan instances or other APIs
-- to enable Vulkan workloads to be split up across application module,
-- process, or API boundaries. This extension enables applications to
-- create exportable Vulkan memory objects such that the underlying
-- resources can be referenced outside the Vulkan instance that created
-- them.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ExternalMemoryImageCreateInfoNV'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ExportMemoryAllocateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_MEMORY_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_MEMORY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) If memory objects are shared between processes and APIs, is this
-- considered aliasing according to the rules outlined in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-memory-aliasing Memory Aliasing>
-- section?
--
-- __RESOLVED__: Yes, but strict exceptions to the rules are added to allow
-- some forms of aliasing in these cases. Further, other extensions may
-- build upon these new aliasing rules to define specific support usage
-- within Vulkan for imported native memory objects, or memory objects from
-- other APIs.
--
-- 2) Are new image layouts or metadata required to specify image layouts
-- and layout transitions compatible with non-Vulkan APIs, or with other
-- instances of the same Vulkan driver?
--
-- __RESOLVED__: No. Separate instances of the same Vulkan driver running
-- on the same GPU should have identical internal layout semantics, so
-- applications have the tools they need to ensure views of images are
-- consistent between the two instances. Other APIs will fall into two
-- categories: Those that are Vulkan compatible (a term to be defined by
-- subsequent interopability extensions), or Vulkan incompatible. When
-- sharing images with Vulkan incompatible APIs, the Vulkan image must be
-- transitioned to the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' layout before
-- handing it off to the external API.
--
-- Note this does not attempt to address cross-device transitions, nor
-- transitions to engines on the same device which are not visible within
-- the Vulkan API. Both of these are beyond the scope of this extension.
--
-- == Examples
--
-- >     // TODO: Write some sample code here.
--
-- == Version History
--
-- -   Revision 1, 2016-08-19 (James Jones)
--
--     -   Initial draft
--
-- == See Also
--
-- 'ExportMemoryAllocateInfoNV', 'ExternalMemoryImageCreateInfoNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_external_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_memory  ( ExportMemoryAllocateInfoNV
                                                , ExternalMemoryImageCreateInfoNV
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExportMemoryAllocateInfoNV

instance ToCStruct ExportMemoryAllocateInfoNV
instance Show ExportMemoryAllocateInfoNV

instance FromCStruct ExportMemoryAllocateInfoNV


data ExternalMemoryImageCreateInfoNV

instance ToCStruct ExternalMemoryImageCreateInfoNV
instance Show ExternalMemoryImageCreateInfoNV

instance FromCStruct ExternalMemoryImageCreateInfoNV

