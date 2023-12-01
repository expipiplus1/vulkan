{-# language CPP #-}
-- | = Name
--
-- VK_EXT_metal_objects - device extension
--
-- == VK_EXT_metal_objects
--
-- [__Name String__]
--     @VK_EXT_metal_objects@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     312
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Bill Hollings
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_metal_objects] @billhollings%0A*Here describe the issue or question you have about the VK_EXT_metal_objects extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_metal_objects.adoc VK_EXT_metal_objects>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Bill Hollings, The Brenwill Workshop Ltd.
--
--     -   Dzmitry Malyshau, Mozilla Corp.
--
-- == Description
--
-- In a Vulkan implementation that is layered on top of Metal on Apple
-- device platforms, this extension provides the ability to import and
-- export the underlying Metal objects associated with specific Vulkan
-- objects.
--
-- As detailed in the
-- <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_metal_objects.adoc extension proposal document>,
-- this extension adds one new Vulkan command, 'exportMetalObjectsEXT', to
-- export underlying Metal objects from Vulkan objects, and supports
-- importing the appropriate existing Metal objects when creating Vulkan
-- objects of types 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.Handles.Image', 'Vulkan.Core10.Handles.Semaphore', and
-- 'Vulkan.Core10.Handles.Event',
--
-- The intent is that this extension will be advertised and supported only
-- on implementations that are layered on top of Metal on Apple device
-- platforms.
--
-- == New Base Types
--
-- -   'IOSurfaceRef'
--
-- -   'MTLBuffer_id'
--
-- -   'MTLCommandQueue_id'
--
-- -   'MTLDevice_id'
--
-- -   'MTLSharedEvent_id'
--
-- -   'MTLTexture_id'
--
-- == New Commands
--
-- -   'exportMetalObjectsEXT'
--
-- == New Structures
--
-- -   'ExportMetalObjectsInfoEXT'
--
-- -   Extending 'ExportMetalObjectsInfoEXT':
--
--     -   'ExportMetalBufferInfoEXT'
--
--     -   'ExportMetalCommandQueueInfoEXT'
--
--     -   'ExportMetalDeviceInfoEXT'
--
--     -   'ExportMetalIOSurfaceInfoEXT'
--
--     -   'ExportMetalSharedEventInfoEXT'
--
--     -   'ExportMetalTextureInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'ImportMetalIOSurfaceInfoEXT'
--
--     -   'ImportMetalTextureInfoEXT'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo',
--     'Vulkan.Core10.Memory.MemoryAllocateInfo',
--     'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo',
--     'Vulkan.Core10.BufferView.BufferViewCreateInfo',
--     'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo',
--     'Vulkan.Core10.Event.EventCreateInfo':
--
--     -   'ExportMetalObjectCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMetalBufferInfoEXT'
--
-- -   Extending 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo',
--     'Vulkan.Core10.Event.EventCreateInfo':
--
--     -   'ImportMetalSharedEventInfoEXT'
--
-- == New Enums
--
-- -   'ExportMetalObjectTypeFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ExportMetalObjectTypeFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_METAL_OBJECTS_EXTENSION_NAME'
--
-- -   'EXT_METAL_OBJECTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_BUFFER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_COMMAND_QUEUE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_DEVICE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_IO_SURFACE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_OBJECTS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_OBJECT_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_SHARED_EVENT_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EXPORT_METAL_TEXTURE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_BUFFER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_IO_SURFACE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_SHARED_EVENT_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_METAL_TEXTURE_INFO_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-05-28 (Bill Hollings)
--
--     -   Initial draft.
--
--     -   Incorporated feedback from review by the Vulkan Working Group.
--         Renamed many structures, moved import\/export of MTLBuffer to
--         VkDeviceMemory, added export of MTLSharedEvent, added import of
--         MTLSharedEvent for VkSemaphore and VkEvent, and changed used bit
--         mask fields to individual bit fields to simplify Valid Usage
--         rules.
--
-- == See Also
--
-- 'IOSurfaceRef', 'MTLBuffer_id', 'MTLCommandQueue_id', 'MTLDevice_id',
-- 'MTLSharedEvent_id', 'MTLTexture_id', 'ExportMetalBufferInfoEXT',
-- 'ExportMetalCommandQueueInfoEXT', 'ExportMetalDeviceInfoEXT',
-- 'ExportMetalIOSurfaceInfoEXT', 'ExportMetalObjectCreateInfoEXT',
-- 'ExportMetalObjectTypeFlagBitsEXT', 'ExportMetalObjectTypeFlagsEXT',
-- 'ExportMetalObjectsInfoEXT', 'ExportMetalSharedEventInfoEXT',
-- 'ExportMetalTextureInfoEXT', 'ImportMetalBufferInfoEXT',
-- 'ImportMetalIOSurfaceInfoEXT', 'ImportMetalSharedEventInfoEXT',
-- 'ImportMetalTextureInfoEXT', 'exportMetalObjectsEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_metal_objects Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_metal_objects  ( ExportMetalBufferInfoEXT
                                               , ExportMetalCommandQueueInfoEXT
                                               , ExportMetalDeviceInfoEXT
                                               , ExportMetalIOSurfaceInfoEXT
                                               , ExportMetalObjectCreateInfoEXT
                                               , ExportMetalObjectsInfoEXT
                                               , ExportMetalSharedEventInfoEXT
                                               , ExportMetalTextureInfoEXT
                                               , ImportMetalBufferInfoEXT
                                               , ImportMetalIOSurfaceInfoEXT
                                               , ImportMetalSharedEventInfoEXT
                                               , ImportMetalTextureInfoEXT
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data ExportMetalBufferInfoEXT

instance ToCStruct ExportMetalBufferInfoEXT
instance Show ExportMetalBufferInfoEXT

instance FromCStruct ExportMetalBufferInfoEXT


data ExportMetalCommandQueueInfoEXT

instance ToCStruct ExportMetalCommandQueueInfoEXT
instance Show ExportMetalCommandQueueInfoEXT

instance FromCStruct ExportMetalCommandQueueInfoEXT


data ExportMetalDeviceInfoEXT

instance ToCStruct ExportMetalDeviceInfoEXT
instance Show ExportMetalDeviceInfoEXT

instance FromCStruct ExportMetalDeviceInfoEXT


data ExportMetalIOSurfaceInfoEXT

instance ToCStruct ExportMetalIOSurfaceInfoEXT
instance Show ExportMetalIOSurfaceInfoEXT

instance FromCStruct ExportMetalIOSurfaceInfoEXT


data ExportMetalObjectCreateInfoEXT

instance ToCStruct ExportMetalObjectCreateInfoEXT
instance Show ExportMetalObjectCreateInfoEXT

instance FromCStruct ExportMetalObjectCreateInfoEXT


type role ExportMetalObjectsInfoEXT nominal
data ExportMetalObjectsInfoEXT (es :: [Type])

instance ( Extendss ExportMetalObjectsInfoEXT es
         , PokeChain es ) => ToCStruct (ExportMetalObjectsInfoEXT es)
instance Show (Chain es) => Show (ExportMetalObjectsInfoEXT es)

instance ( Extendss ExportMetalObjectsInfoEXT es
         , PeekChain es ) => FromCStruct (ExportMetalObjectsInfoEXT es)


data ExportMetalSharedEventInfoEXT

instance ToCStruct ExportMetalSharedEventInfoEXT
instance Show ExportMetalSharedEventInfoEXT

instance FromCStruct ExportMetalSharedEventInfoEXT


data ExportMetalTextureInfoEXT

instance ToCStruct ExportMetalTextureInfoEXT
instance Show ExportMetalTextureInfoEXT

instance FromCStruct ExportMetalTextureInfoEXT


data ImportMetalBufferInfoEXT

instance ToCStruct ImportMetalBufferInfoEXT
instance Show ImportMetalBufferInfoEXT

instance FromCStruct ImportMetalBufferInfoEXT


data ImportMetalIOSurfaceInfoEXT

instance ToCStruct ImportMetalIOSurfaceInfoEXT
instance Show ImportMetalIOSurfaceInfoEXT

instance FromCStruct ImportMetalIOSurfaceInfoEXT


data ImportMetalSharedEventInfoEXT

instance ToCStruct ImportMetalSharedEventInfoEXT
instance Show ImportMetalSharedEventInfoEXT

instance FromCStruct ImportMetalSharedEventInfoEXT


data ImportMetalTextureInfoEXT

instance ToCStruct ImportMetalTextureInfoEXT
instance Show ImportMetalTextureInfoEXT

instance FromCStruct ImportMetalTextureInfoEXT

