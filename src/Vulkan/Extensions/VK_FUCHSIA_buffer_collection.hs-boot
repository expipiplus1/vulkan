{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_buffer_collection - device extension
--
-- == VK_FUCHSIA_buffer_collection
--
-- [__Name String__]
--     @VK_FUCHSIA_buffer_collection@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     367
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_FUCHSIA_external_memory@
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
-- [__Contact__]
--
--     -   John Rosasco
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_FUCHSIA_buffer_collection] @rosasco%0A<<Here describe the issue or question you have about the VK_FUCHSIA_buffer_collection extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Craig Stout, Google
--
--     -   John Bauman, Google
--
--     -   John Rosasco, Google
--
-- == Description
--
-- A buffer collection is a collection of one or more buffers which were
-- allocated together as a group and which all have the same properties.
-- These properties describe the buffers’ internal representation such as
-- its dimensions and memory layout. This ensures that all of the buffers
-- can be used interchangeably by tasks that require swapping among
-- multiple buffers, such as double-buffered graphics rendering.
--
-- By sharing such a collection of buffers between components,
-- communication about buffer lifecycle can be made much simpler and more
-- efficient. For example, when a content producer finishes writing to a
-- buffer, it can message the consumer of the buffer with the buffer index,
-- rather than passing a handle to the shared memory.
--
-- On Fuchsia, the Sysmem service uses buffer collections as a core
-- construct in its design. VK_FUCHSIA_buffer_collection is the Vulkan
-- extension that allows Vulkan applications to interoperate with the
-- Sysmem service on Fuchsia.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA'
--
-- == New Commands
--
-- -   'createBufferCollectionFUCHSIA'
--
-- -   'destroyBufferCollectionFUCHSIA'
--
-- -   'getBufferCollectionPropertiesFUCHSIA'
--
-- -   'setBufferCollectionBufferConstraintsFUCHSIA'
--
-- -   'setBufferCollectionImageConstraintsFUCHSIA'
--
-- == New Structures
--
-- -   'BufferCollectionConstraintsInfoFUCHSIA'
--
-- -   'BufferCollectionCreateInfoFUCHSIA'
--
-- -   'BufferCollectionPropertiesFUCHSIA'
--
-- -   'BufferConstraintsInfoFUCHSIA'
--
-- -   'ImageConstraintsInfoFUCHSIA'
--
-- -   'ImageFormatConstraintsInfoFUCHSIA'
--
-- -   'SysmemColorSpaceFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo':
--
--     -   'BufferCollectionBufferCreateInfoFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'BufferCollectionImageCreateInfoFUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryBufferCollectionFUCHSIA'
--
-- == New Enums
--
-- -   'ImageConstraintsInfoFlagBitsFUCHSIA'
--
-- == New Bitmasks
--
-- -   'ImageConstraintsInfoFlagsFUCHSIA'
--
-- -   'ImageFormatConstraintsFlagsFUCHSIA'
--
-- == New Enum Constants
--
-- -   'FUCHSIA_BUFFER_COLLECTION_EXTENSION_NAME'
--
-- -   'FUCHSIA_BUFFER_COLLECTION_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_BUFFER_COLLECTION_FUCHSIA_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_BUFFER_COLLECTION_FUCHSIA'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_BUFFER_CREATE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_CREATE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_IMAGE_CREATE_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_COLLECTION_PROPERTIES_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_FORMAT_CONSTRAINTS_INFO_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_BUFFER_COLLECTION_FUCHSIA'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SYSMEM_COLOR_SPACE_FUCHSIA'
--
-- == Issues
--
-- 1) When configuring a 'ImageConstraintsInfoFUCHSIA' structure for
-- constraint setting, should a NULL @pFormatConstraints@ parameter be
-- allowed ?
--
-- __RESOLVED__: No. Specifying a NULL @pFormatConstraints@ results in
-- logical complexity of interpreting the relationship between the
-- 'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ settings of the elements
-- of the @pImageCreateInfos@ array and the implied or desired
-- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlags'.
--
-- The explicit requirement for @pFormatConstraints@ to be non-NULL
-- simplifies the implied logic of the implementation and expectations for
-- the Vulkan application.
--
-- == Version History
--
-- -   Revision 2, 2021-09-23 (John Rosasco)
--
--     -   Review passes
--
-- -   Revision 1, 2021-03-09 (John Rosasco)
--
--     -   Initial revision
--
-- == See Also
--
-- 'BufferCollectionBufferCreateInfoFUCHSIA',
-- 'BufferCollectionConstraintsInfoFUCHSIA',
-- 'BufferCollectionCreateInfoFUCHSIA',
-- 'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA',
-- 'BufferCollectionImageCreateInfoFUCHSIA',
-- 'BufferCollectionPropertiesFUCHSIA', 'BufferConstraintsInfoFUCHSIA',
-- 'ImageConstraintsInfoFUCHSIA', 'ImageConstraintsInfoFlagBitsFUCHSIA',
-- 'ImageConstraintsInfoFlagsFUCHSIA',
-- 'ImageFormatConstraintsFlagsFUCHSIA',
-- 'ImageFormatConstraintsInfoFUCHSIA',
-- 'ImportMemoryBufferCollectionFUCHSIA', 'SysmemColorSpaceFUCHSIA',
-- 'createBufferCollectionFUCHSIA', 'destroyBufferCollectionFUCHSIA',
-- 'getBufferCollectionPropertiesFUCHSIA',
-- 'setBufferCollectionBufferConstraintsFUCHSIA',
-- 'setBufferCollectionImageConstraintsFUCHSIA'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_buffer_collection Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_buffer_collection  ( BufferCollectionBufferCreateInfoFUCHSIA
                                                       , BufferCollectionConstraintsInfoFUCHSIA
                                                       , BufferCollectionCreateInfoFUCHSIA
                                                       , BufferCollectionImageCreateInfoFUCHSIA
                                                       , BufferCollectionPropertiesFUCHSIA
                                                       , BufferConstraintsInfoFUCHSIA
                                                       , ImageConstraintsInfoFUCHSIA
                                                       , ImageFormatConstraintsInfoFUCHSIA
                                                       , ImportMemoryBufferCollectionFUCHSIA
                                                       , SysmemColorSpaceFUCHSIA
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BufferCollectionBufferCreateInfoFUCHSIA

instance ToCStruct BufferCollectionBufferCreateInfoFUCHSIA
instance Show BufferCollectionBufferCreateInfoFUCHSIA

instance FromCStruct BufferCollectionBufferCreateInfoFUCHSIA


data BufferCollectionConstraintsInfoFUCHSIA

instance ToCStruct BufferCollectionConstraintsInfoFUCHSIA
instance Show BufferCollectionConstraintsInfoFUCHSIA

instance FromCStruct BufferCollectionConstraintsInfoFUCHSIA


data BufferCollectionCreateInfoFUCHSIA

instance ToCStruct BufferCollectionCreateInfoFUCHSIA
instance Show BufferCollectionCreateInfoFUCHSIA

instance FromCStruct BufferCollectionCreateInfoFUCHSIA


data BufferCollectionImageCreateInfoFUCHSIA

instance ToCStruct BufferCollectionImageCreateInfoFUCHSIA
instance Show BufferCollectionImageCreateInfoFUCHSIA

instance FromCStruct BufferCollectionImageCreateInfoFUCHSIA


data BufferCollectionPropertiesFUCHSIA

instance ToCStruct BufferCollectionPropertiesFUCHSIA
instance Show BufferCollectionPropertiesFUCHSIA

instance FromCStruct BufferCollectionPropertiesFUCHSIA


data BufferConstraintsInfoFUCHSIA

instance ToCStruct BufferConstraintsInfoFUCHSIA
instance Show BufferConstraintsInfoFUCHSIA

instance FromCStruct BufferConstraintsInfoFUCHSIA


data ImageConstraintsInfoFUCHSIA

instance ToCStruct ImageConstraintsInfoFUCHSIA
instance Show ImageConstraintsInfoFUCHSIA

instance FromCStruct ImageConstraintsInfoFUCHSIA


data ImageFormatConstraintsInfoFUCHSIA

instance ToCStruct ImageFormatConstraintsInfoFUCHSIA
instance Show ImageFormatConstraintsInfoFUCHSIA

instance FromCStruct ImageFormatConstraintsInfoFUCHSIA


data ImportMemoryBufferCollectionFUCHSIA

instance ToCStruct ImportMemoryBufferCollectionFUCHSIA
instance Show ImportMemoryBufferCollectionFUCHSIA

instance FromCStruct ImportMemoryBufferCollectionFUCHSIA


data SysmemColorSpaceFUCHSIA

instance ToCStruct SysmemColorSpaceFUCHSIA
instance Show SysmemColorSpaceFUCHSIA

instance FromCStruct SysmemColorSpaceFUCHSIA

